# Design plan: multiple content objects on one slide

Status: finalized local implementation plan, 2026-09-29, incorporating the Astra
review. No runtime implementation has been started. This records the selected
design, not an assertion of upstream maintainer approval.

Issue: [#122](https://github.com/pharmaverse/autoslider.core/issues/122)
Draft PR: [#148](https://github.com/pharmaverse/autoslider.core/pull/148)
Code reviewed: `5ae89303` (the claim placeholder on top of `9a300d42`).

## Selected scope

Start with Option B from the issue: keep one primary program per spec entry and
let the caller supply one additional content object. A two-content slide must
contain exactly one page from each object, in two explicit, distinct placeholders.
The primary output owns the slide title and slide-level metadata.

The first release should support two tables, a table and a plot in either order,
and two plots. It should use an existing template layout rather than arrange or
resize the layout itself. Existing single-output slides retain their behavior.

This is a supported composition interface around existing rendering capabilities.
It does not require new analysis programs. The secondary object is created by the
caller, potentially using an existing output-generating program.

## Findings from the current code

The extra-argument loops in `table_to_slide()` and `figure_to_slide()` already call
`ph_with(ppt, value = x$value, location = x$location)` without restricting the value
to text. Simply extracting that loop into a helper does not add multi-content
support. The earlier implementation was insufficient for this issue.

The gaps are in configuration, conversion, placement, and validation:

| Area | Current behavior and design consequence |
| --- | --- |
| Output generation | `generate_outputs()` runs one program and attaches its spec. Preserve that contract. `args` belongs to the analysis program, not slide rendering. |
| Spec handling | `read_spec()` expands suffixes and retains extra fields. A secondary object must be bound to the intended expanded entry. |
| Decoration | The table path restores the spec; the ggplot/grob branches do not explicitly restore it. Composition metadata must survive every supported branch. |
| Slide settings | `generate_slides()` reads per-output font/format settings, but does not generally resolve layout/location/extra-content settings from the spec. |
| Argument forwarding | Extra `value`/`location` arguments can reach table formatters through `...`. Some explicit locations can also collide with internally supplied defaults. Resolve slide arguments once and keep them out of conversion arguments. |
| Figure placement | The editable figure path hardcodes the body placeholder; the image path uses explicit full-slide figure dimensions. Both need to honor the selected panel. |
| Table placement | Several table paths center against the whole slide. Table dimensions must instead be evaluated against the target panel. |
| Pagination | Tables/listings and decorated plot sets may produce multiple slides. A composed slide needs explicit handling of this case. |
| Return values | Some slide helpers return the result of `lapply()` rather than the presentation. Any touched helper should return the updated `rpptx`, and its caller should use that return value. |

The bundled `inst/theme/basic.pptx` was inspected: its `Two Content` layout has
`Content Placeholder 2` and `Content Placeholder 3`. These are names, not an API
promise that every study template uses the same labels.

## Options and tradeoffs

| Option | Benefit | Limitation | Recommendation |
| --- | --- | --- | --- |
| A: pre-arrange plots into one graphic | Already possible for plots | Does not provide independently positioned native tables and plots | Keep as an existing alternative |
| B: primary output plus supplied secondary content | Smallest extension that retains the output pipeline | Caller must create/bind the secondary object; YAML does not run a second program | Selected scope for #122 |
| C: a `panels` spec with multiple programs | Full declarative composition with independent filters and arguments | Changes generation, decoration, saving, and error handling | Separate follow-up if maintainers accept B first |

If the requirement is that YAML alone must invoke both programs, Option B does
not meet it. That would be a deliberate decision to implement Option C, not a
small extension hidden inside `...`.

## Planned interface

All names and examples below describe the proposal, not currently available APIs.

Add a `slide_content()` helper that packages a secondary object, its location,
and its rendering options. It creates a descriptor without opening a graphics
device or writing an image. Conversion happens during slide generation, when the
template and placeholder dimensions are known. This avoids temporary image paths
that expire between spec creation, saving, and rendering.

The descriptor contains `value`, a nonempty placeholder-label string `location`,
and optional `fig_editable = NULL`. A NULL graphics setting inherits the explicit
deck setting, then the existing FALSE default. It must not run a program,
re-filter data, or resolve an output by name. Table styling and geometry belong
to the supplied flextable; no primary-table formatting is inherited.

Use an `extra_content` field on the spec entry for this descriptor. In the first
release it holds one descriptor directly, without a singleton list. Collections
raise an informative error. Keeping it separate from `args` prevents it reaching analysis functions.
Preserve legacy extra `value`/`location` arguments for compatibility.

Example YAML for the primary output:

```yaml
- program: t_dm_slide
  suffix: FAS
  titles: Demographics and age distribution
  footnotes: Demographics summary
  paper: L6
  layout: Two Content
  table_loc: !expr officer::ph_location_label("Content Placeholder 2")
  args:
    arm: TRT01A
    vars: [SEX, AGE]
```

The caller then binds an already-created plot to the chosen expanded spec entry:

```r
# Proposed API. age_plot is a caller-created ggplot for the intended population.
spec <- read_spec("spec.yml")
stopifnot("t_dm_slide_FAS" %in% names(spec))

spec[["t_dm_slide_FAS"]]$extra_content <- slide_content(
  value = age_plot,
  location = "Content Placeholder 3"
)

spec |>
  generate_outputs(datasets = datasets) |>
  decorate_outputs() |>
  generate_slides(outfile = "demographics.pptx")
```

Supplying a second table uses the same descriptor with a prepared flextable. It is not a
second element in the top-level outputs list: that list continues to describe
separate primary outputs. The caller is responsible for applying the intended
population/filter to the secondary output. Binding happens after suffix expansion
so one object is not accidentally reused across different populations.

For direct R use, propose an explicit `extra_content = NULL` argument to
`generate_slides()`. A nonempty value is allowed only when the normalized input
contains one primary output; use per-entry specs for multiple outputs. Reject
simultaneous spec and argument definitions of `extra_content` as ambiguous.

For composed slides, resolve per-entry `layout`, optional `master`, `table_loc`
or `figure_loc` ahead of explicit call settings, then existing defaults. Require
explicit primary and secondary content locations even when a layout default is
available. Primary formatting retains its own spec/deck precedence. Secondary
tables preserve the formatting explicitly applied before composition.

## Content and layout contract

### Supported content

The secondary descriptor accepts a prepared `flextable`, caller-owned
`external_img`, or self-contained ggplot/grob. It rejects raw analysis tables,
decorated output wrappers, `autoslider_error`, and output collections. A secondary
plot must already contain any required caption and footnotes in its drawn content.
Honor `fig_editable` for ggplots/grobs; reject an editable request for an external
image instead of silently claiming it is editable.

Provide `as_slide_flextable(x, ...)` as a separate one-page conversion helper for
data frames, rtables, listings, and gtsummary outputs. It returns a flextable,
preserves applicable table footnotes, and rejects multiple pages. Formatting
resolves from explicit helper arguments, then the source object's own spec, then
type defaults. It has no implicit dependency on the primary object. Callers can
adjust its dimensions using flextable functions before passing it to `slide_content`.

| Input | Primary preparation / table helper | Secondary descriptor |
| --- | --- | --- |
| flextable | Preserve styling and geometry; one supplied object | Accepted directly |
| data.frame | Convert as a small table; validate resulting size | Convert with table helper first |
| raw/decorated rtables or listing | Normalize through the same pagination path; retain all page results until validation | Convert with table helper first |
| raw/decorated gtsummary | Normalize through the same pagination path; retain all page results until validation | Convert with table helper first |
| ggplot/grob | Prepare one graphic | Accepted, including its drawn captions |
| single decoratedGrob | Primary only; preserve primary metadata separately from graphic | Rejected; supply a self-contained graphic |
| decoratedGrobSet, errors, arbitrary lists | Rejected for composition | Rejected |

Dispatch listing classes before data frames and decorated classes before their
base classes. Explicit `lpp`/`cpp` settings govern pagination where supported;
otherwise use the existing deck defaults for primary conversion and matching
defaults in the table helper. Raw and decorated variants must use equivalent
pagination inputs. Check physical fit separately; do not reuse whole-slide
height heuristics as panel height. No preview function or implicit first-page
selection is permitted. A supplied flextable has no recoverable pagination
history: validate its actual size, without claiming to detect rows removed by
the caller before it was supplied.

### Placement and sizing

Require explicit named content placeholders for both panels in the first
release. Validate names against the selected layout/master, not against all
layouts in the template. When a layout occurs in multiple masters, require
`master`; when it is unique, infer the matching master. Fail if either label is
missing, ambiguous, reused, or resolves to a title/footer placeholder. Reject
overlapping panel rectangles in this initial supported interface.

Resolve both panel rectangles before adding the slide. The primary location
must override automatic whole-slide centering. Both editable and image figure
paths must use their resolved panel bounds. Preserve the aspect ratio of existing
images by fitting them within the panel; draw plots at the panel dimensions.

Native flextables retain their prepared column widths, row sizing, and font sizes;
a placeholder does not resize them. Compare configured dimensions and supported
content-size estimates against the panel, including headers and footnotes. Reject
overflow with the measured size and available bounds. Do not automatically reflow
columns or shrink fonts. The caller prepares a smaller table, chooses a suitable
layout, or uses separate slides. The implementation must test its chosen dimension
measurement against wrapped text and merged cells before declaring support.

PowerPoint may render fonts differently, so measured bounds are a preflight
check, not a guarantee of pixel-identical output. Rendered examples remain part
of acceptance testing.

### Pagination and errors

For any slide using the new composition interface, both primary and secondary
objects must produce exactly one page. Reject zero or multiple pages. Do not
discard later pages, repeat the other panel, or silently produce continuation
slides. Existing standalone pagination is unchanged.

This is a deliberate design decision: issue #122 leaves pagination unresolved.
Automatically showing only the first page could omit clinically relevant results
while making the slide appear complete. There is no implicit truncation or
`first_page` option in v1. A caller may deliberately create and label an excerpt
before composition; the package must not silently create that excerpt.

Prepare and validate both panels before adding the slide. On failure, stop with
the output identifier, primary/secondary panel, and the relevant page count,
placeholder, or size. Do not write a partial deck over `outfile`. Clean up any
temporary graphics files on success and error, after they have been embedded.

Example diagnostic: `t_dm_slide_FAS: secondary content requires 3 pages; a
two-content slide requires one page per panel. Reduce the content or use separate
slides.`

### Titles, footnotes, and notes

The primary output supplies the single slide title, generic study metadata,
version label, and slide notes. A secondary object must not overwrite that title
or create another slide-level confidentiality line.

Secondary metadata must be self-contained: captions and footnotes are drawn in
the plot or included in flextable header/footer rows. The table helper preserves
source footnotes and provenance in those rows. A flextable caption attribute alone
is not a visible PowerPoint caption. No secondary speaker-note merging or generic
footer deduplication is attempted in v1.

For the primary panel, retain a composition-specific metadata record containing
the resolved slide title, panel footnotes, and speaker notes before conversion.
Capture graph titles/footnotes from the spec during composition-aware decoration,
including token expansion, rather than expecting graphic insertion to infer them.
For direct decorated inputs without a spec, use their stored fields. Render primary
graph footnotes in a reserved strip inside the primary panel; table footnotes remain
in table rows. Include the strip in geometry checks. Already-flattened metadata is
preserved as supplied, without attempting to infer which strings are duplicates.
Existing single-output decoration behavior is outside this change.

Preserve all existing title/subtitle/footer text arguments. If an explicit text
argument targets one of the occupied panel placeholders, report the collision
instead of covering the content.

### Saved outputs

Retain the descriptor and spec through decoration and RDS saving/loading.
Descriptors hold content objects rather than generated temporary images. A
caller-supplied `external_img` still depends on its source file being available
when slides are built; validate this and report a missing file explicitly.

The inspected `save_outputs()` paths save supported decorated outputs as RDS.
Verify that their attached descriptor survives this existing mechanism, including
`slides_from_rds()`. Composition does not introduce a general export object or new
PDF/image export capabilities.

## Implementation sequence

1. Introduce and document the content descriptor and one-page table converter,
   including supported classes and explicit sizing. Keep RDS round-tripping free
   of transient rendering files.
2. Resolve per-entry slide settings and preserve them through table and graph
   decoration. Keep slide settings and legacy located arguments out of analysis
   and table-formatting arguments.
3. Add a composition-specific renderer: resolve locations, prepare both panels,
   validate them, add one slide, and insert both contents plus primary decoration.
   Reuse conversion primitives, not helpers that each add a slide. Return and use
   the updated presentation consistently. Keep ordinary output dispatch stable;
   any shared behavior correction requires its own explicit regression coverage.
4. Add end-to-end examples and focused regression tests, then run the package
   checks. Remove `.github/issue-122-claim.md` before the final PR is merged.

Likely source areas: `R/to_slides.R`, `R/to_ft_funs.R`, and `R/decorate.R`, plus
helper documentation, tests, and a vignette. Preserve `generate_outputs()`'s
one-program contract; verify spec and saving behavior rather than redesigning
them. Do not introduce new study analysis programs for this feature.

## Acceptance and validation plan

| Scenario | Required evidence |
| --- | --- |
| Two tables; table plus plot; plot plus table; two plots | One added slide with both distinct objects at the intended panel bounds |
| Native tables and editable figures | Serialized PPTX has native table/graphic content; both editable and image graph paths honor placement |
| Primary and secondary pagination | One page succeeds; multiple pages fail without writing a partial deck |
| Oversized tables and long footnotes | Overflow reported; no silent truncation or automatic unreadable font reduction |
| Missing, duplicate, or ambiguous placeholders/master | Error identifies the offending output and location |
| Spec pipeline and direct R entry | Composition reaches rendering without leaking into program or formatter arguments |
| Suffix expansion and saved outputs | Correct entry gets its secondary object; RDS round-trip preserves it |
| Decoration | One slide title; primary metadata rendered; supplied secondary captions/footnotes retained; primary notes retained |
| Secondary formatting | Prepared styling preserved; table helper honors the source object's own spec rather than the primary's |
| Raw/decorated equivalence | Equivalent table inputs use identical pagination settings and do not bypass multi-page rejection |
| Existing single-object usage | Existing pagination, title/subtitle arguments, font defaults, and plot-set behavior still pass |
| Error cleanup | Temporary image resources removed; existing destination deck remains intact |

Inspect serialized PPTX relationships, content, and shape bounds rather than
testing slide count alone. Visually inspect a small exported example deck for
clipping and readability using the bundled template and, if supplied, one study
template. Run focused tests, `devtools::test()`, `lintr::lint_package()`, and
`devtools::check()` once implementation exists and dependencies are available.

## Final decisions and boundaries

The local plan selects narrow Option B, `slide_content`/`extra_content`, an explicit
one-page table conversion helper, caller binding by expanded spec name, named
placeholders, and rejection of pagination/overflow. Secondary content is prepared
by the caller; its formatting is independent of the primary. Implementation can
be planned against this contract without choosing further public behavior.

Before shipping, maintainers still need to accept that R-side binding satisfies
the initial issue. If they require YAML to invoke and filter both programs, move
to Option C and reuse the two-panel renderer. This document does not claim that
upstream has approved that scope or the pagination decision.

Deferred work: nested programs, cross-output references, automatic table fitting,
secondary decorated-graph metadata reconstruction, generic-footer deduplication,
secondary speaker-note merging, paired pagination, automatic layout selection,
arbitrary coordinate placement, and arrangement of more than two objects.

## Sources

- [Issue #122](https://github.com/pharmaverse/autoslider.core/issues/122): goal,
  options, and proposed success criteria.
- Repository files reviewed: `R/spec.R`, `R/generate_output.R`,
  `R/func_wrapper.R`, `R/decorate.R`, `R/to_slides.R`, `R/to_ft_funs.R`,
  `R/save_output.R`, and the bundled `basic.pptx` layout XML.
- [officer: ph_with](https://davidgohel.github.io/officer/reference/ph_with.html):
  existing support for adding different value types to a slide and image sizing.
- [officer: ph_location_label](https://davidgohel.github.io/officer/reference/ph_location_label.html):
  targeting a named placeholder.
- [flextable: PowerPoint insertion](https://davidgohel.github.io/flextable/reference/ph_with.flextable.html):
  flextable sizing is controlled by the table, not the placeholder dimensions.
