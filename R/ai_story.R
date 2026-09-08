# AI "story" content slides
# ------------------------------------------------------------------------------
# A post-processing step that complements `get_ai_notes()` (which attaches an
# LLM response as *speaker notes*, easy to miss and never shown in presentation
# mode). Instead, this loads an already-generated deck, sends the underlying
# table data to an LLM asking it to "tell a story", and inserts the narrative as
# REAL content slides (title + bullets on an AI-chosen layout): a short summary
# section at the front of the deck and a conclusions section at the end.
#
# The design keeps the LLM call (`get_ai_story()`) separate from the pure
# `officer` slide insertion (`add_story_slides()`) so the bulk of the logic is
# unit-testable without any network access.

# Layouts the AI is allowed to pick from. Deliberately a small, text-only subset
# so the narrative always renders regardless of the template's design layouts.
.story_safe_layouts <- c("Section Header", "Title and Content", "Title Only")

#' Layouts available for AI story slides
#'
#' Intersects the safe text-only layout subset with the layouts the template
#' actually ships, so every layout offered to the LLM is guaranteed to exist.
#'
#' @param ppt An `officer::rpptx` object.
#'
#' @return A character vector of layout names present in `ppt`.
#'
#' @noRd
story_layout_choices <- function(ppt) {
  available <- officer::layout_summary(ppt)$layout
  choices <- intersect(.story_safe_layouts, available)
  if (length(choices) == 0) {
    # Fall back to whatever the template ships so we never offer an empty set.
    choices <- available
  }
  choices
}

#' Build the deck-level story prompt from decorated outputs
#'
#' Collects the rendered text of each decorated table output (reusing
#' `export_as_txt()`, exactly as `integrate_prompt()` does for AI notes) and
#' assembles one instruction asking the LLM to produce a short summary section
#' and a conclusions section, each slide carrying a title, concise bullets and a
#' layout chosen from `allowed_layouts`.
#'
#' Outputs that errored (`autoslider_error`) or that are not tables (no `tbl`
#' slot, e.g. figures/listings) are skipped.
#'
#' @param outputs A named `list` of decorated outputs (as returned by
#'   [decorate_outputs()]).
#' @param allowed_layouts Character vector of permitted slide layouts.
#' @param max_slides Integer cap on the number of slides per section.
#'
#' @return A single character string: the prompt.
#'
#' @noRd
build_story_prompt <- function(outputs, allowed_layouts, max_slides = 4L) {
  tables <- list()
  for (nm in names(outputs)) {
    out <- outputs[[nm]]
    if (is(out, "autoslider_error")) {
      next
    }
    if (!methods::.hasSlot(out, "tbl")) {
      next
    }
    txt <- tryCatch(
      paste(export_as_txt(out@tbl), collapse = "\n"),
      error = function(e) ""
    )
    if (nzchar(txt)) {
      tables[[nm]] <- txt
    }
  }

  tables_block <- paste(
    vapply(
      names(tables),
      function(nm) paste0("### ", nm, "\n", tables[[nm]]),
      character(1)
    ),
    collapse = "\n\n"
  )

  paste0(
    "You are a Clinical data scientist expert preparing a slide deck for a ",
    "study team. Below are the tables from a generated deck.\n\n",
    tables_block,
    "\n\n",
    "Tell the story of these data as presentation slides. Produce two sections:\n",
    "1. `summary`: up to ", max_slides, " slides that open the deck and give the ",
    "audience the key take-aways before they see the detailed tables.\n",
    "2. `conclusions`: up to ", max_slides, " slides that close the deck with the ",
    "main conclusions and any caveats.\n\n",
    "For every slide provide a short title, a handful of concise bullet points, ",
    "and choose a `layout` from exactly this set: ",
    paste(allowed_layouts, collapse = ", "), ". ",
    "Use a section-divider layout (e.g. 'Section Header') for the slide that ",
    "opens each section, and a content layout for slides that carry bullets. ",
    "Keep bullets factual and grounded in the numbers above."
  )
}

#' Fallback: get the story as plain-text JSON and parse it
#'
#' Used when a provider does not support `chat$chat_structured()`. Asks the model
#' to reply with a bare JSON object matching the story schema, strips any
#' reasoning block or markdown fence, extracts the outermost JSON object and
#' parses it. Returns a `list(summary, conclusions)`; missing sections default to
#' empty lists.
#'
#' @param chat An `ellmer` chat object.
#' @param prompt The deck-level story prompt (see [build_story_prompt()]).
#' @param allowed_layouts Character vector of permitted layouts (named in the
#'   JSON instruction so the model stays in range).
#'
#' @return A `list` with `summary` and `conclusions` elements.
#'
#' @noRd
story_via_json <- function(chat, prompt, allowed_layouts) {
  instruction <- paste0(
    prompt, "\n\n",
    "Respond with ONLY a JSON object (no markdown fences, no commentary) of the form:\n",
    '{"summary":[{"layout":"<one of: ', paste(allowed_layouts, collapse = " | "),
    '>","title":"a short title","bullets":["a bullet","another bullet"]}],',
    '"conclusions":[{"layout":"...","title":"...","bullets":["..."]}]}'
  )
  raw <- chat$chat(instruction, echo = "none")
  txt <- sub(".*?</think>\\s*", "", raw) # drop <think>...</think> reasoning blocks
  match <- regmatches(txt, regexpr("(?s)\\{.*\\}", txt, perl = TRUE))
  if (length(match) == 1L) {
    txt <- match
  }
  parsed <- jsonlite::fromJSON(txt, simplifyDataFrame = FALSE)
  list(
    summary = if (is.null(parsed$summary)) list() else parsed$summary,
    conclusions = if (is.null(parsed$conclusions)) list() else parsed$conclusions
  )
}

#' Ask an LLM to tell the story of the decorated outputs
#'
#' Sends the deck's tables to the chosen LLM provider and returns a structured
#' narrative: a `summary` section (for the front of the deck) and a
#' `conclusions` section (for the end). Each section is a list of slides, and
#' each slide has a `layout`, a `title` and a character vector of `bullets`.
#'
#' @param outputs A named `list` of decorated outputs (see [decorate_outputs()]).
#' @param allowed_layouts Character vector of permitted layouts. Any layout the
#'   model returns outside this set is clamped to `"Title and Content"`.
#' @param platform,base_url,api_key,model Passed to [get_ellmer_chat()] to build
#'   the chat connection.
#' @param max_slides Integer cap on slides per section.
#'
#' @return A `list` with two elements, `summary` and `conclusions`, each a list
#'   of slides (`list(layout, title, bullets)`).
#'
#' @export
get_ai_story <- function(outputs,
                         allowed_layouts,
                         platform = "deepseek",
                         base_url = "https://api.deepseek.com",
                         api_key = get_deepseek_key(),
                         model = "deepseek-chat",
                         max_slides = 4L) {
  chat <- get_ellmer_chat(platform, base_url, api_key, model)

  slide_type <- ellmer::type_object(
    layout = ellmer::type_enum(
      allowed_layouts,
      "The slide layout to use; must be one of the allowed layouts."
    ),
    title = ellmer::type_string("A short slide title."),
    bullets = ellmer::type_array(
      ellmer::type_string("A single concise bullet point."),
      "The bullet points shown on the slide body."
    )
  )
  story_type <- ellmer::type_object(
    summary = ellmer::type_array(
      slide_type,
      "Slides that open the deck with the key take-aways."
    ),
    conclusions = ellmer::type_array(
      slide_type,
      "Slides that close the deck with the main conclusions."
    )
  )

  prompt <- build_story_prompt(outputs, allowed_layouts, max_slides)

  # Prefer the provider's native structured output. Not every provider supports
  # it (e.g. DeepSeek returns HTTP 400 for the request `ellmer` emits), so fall
  # back to plain-text JSON mode and parse the response ourselves.
  story <- tryCatch(
    chat$chat_structured(prompt, type = story_type),
    error = function(e) story_via_json(chat, prompt, allowed_layouts)
  )

  # Coerce every slide into a clean `list(layout, title, bullets)` regardless of
  # which path produced it, clamping any layout the model invented back into the
  # allowed set so `add_story_slides()` can always find a matching layout.
  clamp <- function(slides) {
    if (is.null(slides)) {
      return(list())
    }
    lapply(slides, function(s) {
      layout <- if (is.null(s$layout)) "" else as.character(s$layout)[1]
      if (!(layout %in% allowed_layouts)) {
        layout <- "Title and Content"
      }
      list(
        layout = layout,
        title = if (is.null(s$title)) "" else as.character(s$title)[1],
        bullets = if (is.null(s$bullets)) character(0) else as.character(unlist(s$bullets))
      )
    })
  }

  # Drop slides the model left empty (no title and no bullets) so the deck has
  # no blank inserts.
  drop_empty <- function(slides) {
    Filter(function(s) nzchar(s$title) || length(s$bullets) > 0, slides)
  }

  list(
    summary = drop_empty(clamp(story$summary)),
    conclusions = drop_empty(clamp(story$conclusions))
  )
}

#' Insert a single story slide into a deck
#'
#' Adds one slide (title + optional bullets) on the requested layout. If the
#' layout has no body placeholder (e.g. `"Title Only"`) but the slide carries
#' bullets, the layout is coerced to `"Title and Content"` so no content is
#' dropped.
#'
#' @param ppt An `officer::rpptx` object.
#' @param slide A single slide definition (`list(layout, title, bullets)`).
#' @param layouts A `data.frame` from [officer::layout_summary()] mapping each
#'   layout to its master.
#'
#' @return The modified `officer::rpptx` object (slide appended at the end).
#'
#' @noRd
add_one_story_slide <- function(ppt, slide, layouts) {
  layout <- slide$layout
  bullets <- if (is.null(slide$bullets)) character(0) else slide$bullets

  # A layout with no body placeholder cannot hold bullets: promote to a content
  # layout so the narrative survives.
  has_body <- function(lay) {
    props <- officer::layout_properties(ppt, layout = lay)
    any(props$type == "body")
  }
  if (length(bullets) > 0 && !has_body(layout) && "Title and Content" %in% layouts$layout) {
    layout <- "Title and Content"
  }

  master <- layouts$master[match(layout, layouts$layout)][1]
  ppt <- officer::add_slide(ppt, layout = layout, master = master)

  if (!is.null(slide$title) && nzchar(slide$title)) {
    ppt <- officer::ph_with(
      ppt,
      value = slide$title,
      location = officer::ph_location_type(type = "title")
    )
  }
  if (length(bullets) > 0 && has_body(layout)) {
    ppt <- officer::ph_with(
      ppt,
      value = officer::unordered_list(
        str_list = bullets,
        level_list = rep(1L, length(bullets))
      ),
      location = officer::ph_location_type(type = "body")
    )
  }
  ppt
}

#' Insert AI story slides into an open deck
#'
#' Inserts the `summary` slides at the front of the deck (in order) and the
#' `conclusions` slides at the end. This is pure `officer` work with no network
#' access, so it can be unit-tested with a hand-written `story`.
#'
#' @param ppt An `officer::rpptx` object holding the generated content deck.
#' @param story A story `list` with `summary` and `conclusions` elements, each a
#'   list of slides (see [get_ai_story()]).
#'
#' @return The modified `officer::rpptx` object.
#'
#' @export
add_story_slides <- function(ppt, story) {
  layouts <- officer::layout_summary(ppt)

  # Conclusions first: appended to the end, where they stay.
  conclusions <- if (is.null(story$conclusions)) list() else story$conclusions
  for (slide in conclusions) {
    ppt <- add_one_story_slide(ppt, slide, layouts)
  }

  # Summary next: each new slide is appended at the end, then relocated to its
  # target position at the front. Moving them front-to-back in order reproduces
  # the intended summary order ahead of all content slides.
  summary <- if (is.null(story$summary)) list() else story$summary
  for (j in seq_along(summary)) {
    ppt <- add_one_story_slide(ppt, summary[[j]], layouts)
    ppt <- officer::move_slide(ppt, index = length(ppt), to = j)
  }

  ppt
}

#' Add an AI-generated story to a generated deck
#'
#' Post-processes an already-generated `.pptx`: loads it, asks an LLM to tell the
#' story of the decorated outputs (see [get_ai_story()]), and inserts the
#' narrative as real content slides -- a summary section at the front and a
#' conclusions section at the end -- then writes the deck back out.
#'
#' Unlike [get_ai_notes()], which hides the LLM response in speaker notes, this
#' produces slides that are visible in presentation mode.
#'
#' @param outputs A named `list` of decorated outputs (see [decorate_outputs()]),
#'   used as the data the story is told from.
#' @param infile Path to the generated `.pptx` to read.
#' @param outfile Path to write the augmented deck to. Defaults to `infile`
#'   (overwrite in place).
#' @param platform,base_url,api_key,model Passed to [get_ellmer_chat()].
#' @param max_slides Integer cap on slides per section.
#'
#' @return Invisibly, the path written (`outfile`).
#'
#' @export
add_ai_story <- function(outputs,
                         infile,
                         outfile = infile,
                         platform = "deepseek",
                         base_url = "https://api.deepseek.com",
                         api_key = get_deepseek_key(),
                         model = "deepseek-chat",
                         max_slides = 4L) {
  assert_that(is.list(outputs), msg = "`outputs` must be a list of decorated outputs.")
  assert_that(
    is.string(infile) && file.exists(infile),
    msg = "`infile` must be the path to an existing .pptx file."
  )

  ppt <- officer::read_pptx(infile)
  allowed_layouts <- story_layout_choices(ppt)
  story <- get_ai_story(
    outputs,
    allowed_layouts = allowed_layouts,
    platform = platform,
    base_url = base_url,
    api_key = api_key,
    model = model,
    max_slides = max_slides
  )
  ppt <- add_story_slides(ppt, story)
  print(ppt, target = outfile)

  invisible(outfile)
}
