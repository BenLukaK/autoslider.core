confidential_footnote <- "Confidential and for internal use only"
default_footer_font_size <- 8L

make_footnote_value <- function(value, font_size = NULL) {
  if (is.null(font_size)) {
    return(as_paragraph(value))
  }

  assertthat::assert_that(
    is.numeric(font_size),
    length(font_size) == 1,
    !is.na(font_size)
  )
  as_paragraph(as_chunk(value, props = fp_text(font.size = font_size)))
}

#' Describe secondary content for a multi-content slide
#'
#' `slide_content()` describes one caller-supplied object that will share a slide
#' with a primary autoslider output. The object is rendered only when the slide is
#' generated, after the requested placeholder has been resolved in the template.
#'
#' @param value A prepared `flextable`, `external_img`, `ggplot`, or grid grob.
#' @param location Name of a content placeholder in the selected layout.
#' @param fig_editable Whether a ggplot should be added as editable DrawingML.
#'   `NULL` inherits the deck setting.
#' @return An object of class `slide_content`.
#' @export
slide_content <- function(value, location, fig_editable = NULL) {
  if (!is.character(location) || length(location) != 1L || is.na(location) || !nzchar(location)) {
    stop("`location` must be one non-empty placeholder label.", call. = FALSE)
  }
  if (!is.null(fig_editable) && (!is.logical(fig_editable) || length(fig_editable) != 1L || is.na(fig_editable))) {
    stop("`fig_editable` must be NULL or one non-missing logical value.", call. = FALSE)
  }

  structure(
    list(value = value, location = location, fig_editable = fig_editable),
    class = "slide_content"
  )
}

#' Convert one table-like object for use as secondary slide content
#'
#' @param x A table-like object or a prepared `flextable`.
#' @param lpp,cpp Pagination settings used for table and listing inputs.
#' @param table_format Optional formatter for converted tables.
#' @param ... Arguments passed to [to_flextable()].
#' @return A single `flextable`.
#' @export
as_slide_flextable <- function(x, lpp = 20, cpp = 200, table_format = NULL, ...) {
  if (inherits(x, "flextable")) {
    return(x)
  }

  sp <- attr(x, "spec") %||% list()
  if (is.null(table_format)) {
    table_format <- sp$table_format
  }
  if (is.null(table_format)) {
    table_format <- if (inherits(x, c("gtsummary", "tbl_roche_summary", "dgtsummary"))) {
      autoslider_format
    } else {
      orange_format
    }
  }
  fs <- sp$font_size %||% list()
  table_format <- with_font_sizes(table_format, fs$body, fs$header, fs$footer)

  out <- if (inherits(x, "dflextable")) {
    x
  } else if (inherits(x, c("dVTableTree", "VTableTree"))) {
    to_flextable(x, lpp = lpp, cpp = cpp, table_format = table_format, ...)
  } else if (inherits(x, "dlisting")) {
    to_flextable(x, lpp = lpp, cpp = cpp, ...)
  } else if (inherits(x, "dgtsummary")) {
    to_flextable(x, lpp = lpp, table_format = table_format, ...)
  } else {
    to_flextable(x, table_format = table_format, ...)
  }
  if (inherits(out, "dflextable")) {
    if (length(out) != 1L) {
      stop(
        "Secondary table content requires more than one page; a multi-content slide requires one page per panel.",
        call. = FALSE
      )
    }
    out <- out[[1]]$ft
  }

  if (!inherits(out, "flextable")) {
    stop("`x` could not be converted to one flextable for slide content.", call. = FALSE)
  }

  out
}

is_slide_content <- function(x) inherits(x, "slide_content")

validate_slide_content <- function(x) {
  if (!is_slide_content(x)) {
    stop("`extra_content` must be created with `slide_content()`.", call. = FALSE)
  }
  if (is.list(x$value) && !inherits(x$value, c("flextable", "external_img", "ggplot", "grob"))) {
    stop("`slide_content()` accepts one prepared flextable, external image, ggplot, or grob.", call. = FALSE)
  }
  if (inherits(x$value, c("decoratedGrob", "decoratedGrobSet", "autoslider_error"))) {
    stop(
      "Decorated output wrappers cannot be secondary content; supply a prepared flextable or self-contained graphic.",
      call. = FALSE
    )
  }
  if (!inherits(x$value, c("flextable", "external_img", "ggplot", "grob"))) {
    stop("`slide_content()` accepts one prepared flextable, external image, ggplot, or grob.", call. = FALSE)
  }
  if (inherits(x$value, "external_img") && isTRUE(x$fig_editable)) {
    stop("An external image cannot be inserted as editable content.", call. = FALSE)
  }
  x
}

resolve_slide_master <- function(ppt, layout, master = NULL, require_unique = FALSE) {
  layouts <- layout_summary(ppt)
  choices <- layouts[layouts$layout == layout, , drop = FALSE]
  if (!nrow(choices)) {
    stop(sprintf("Layout `%s` is not available in the template.", layout), call. = FALSE)
  }
  if (!is.null(master)) {
    if (!is.character(master) || length(master) != 1L || !master %in% choices$master) {
      stop(sprintf("Layout `%s` is not available in master `%s`.", layout, master), call. = FALSE)
    }
    return(master)
  }

  masters <- unique(choices$master)
  if (require_unique && length(masters) != 1L) {
    stop(sprintf("Layout `%s` appears in multiple masters; specify `master`.", layout), call. = FALSE)
  }
  masters[[1]]
}

composite_panel_properties <- function(ppt, layout, master, label, role) {
  props <- officer::layout_properties(ppt, layout = layout, master = master)
  panel <- props[props$ph_label == label, , drop = FALSE]
  if (nrow(panel) != 1L) {
    stop(
      sprintf(
        "The %s placeholder `%s` is not available exactly once in layout `%s` / master `%s`.",
        role, label, layout, master
      ),
      call. = FALSE
    )
  }
  panel
}

panel_size <- function(panel) {
  list(width = as.numeric(panel$cx[[1]]), height = as.numeric(panel$cy[[1]]))
}

validate_flextable_fits_panel <- function(ft, panel, role) {
  dims <- flextable::flextable_dim(ft)
  size <- panel_size(panel)
  width <- sum(dims$widths)
  height <- sum(dims$heights)
  if (width > size$width || height > size$height) {
    stop(
      sprintf(
        paste0(
          "%s table (%.2f x %.2f in) does not fit its placeholder (%.2f x %.2f in). ",
          "Resize the table, choose another layout, or use separate slides."
        ),
        role, width, height, size$width, size$height
      ),
      call. = FALSE
    )
  }
}

insert_slide_content <- function(ppt, content, panel, fig_editable = FALSE) {
  value <- content$value
  if (inherits(value, "flextable")) {
    validate_flextable_fits_panel(value, panel, "Secondary")
    return(ph_with(ppt, value = value, location = ph_location_label(content$location)))
  }
  if (inherits(value, "external_img")) {
    return(ph_with(ppt, value = value, location = ph_location_label(content$location)))
  }
  if (inherits(value, "ggplot")) {
    if (isTRUE(fig_editable)) {
      return(ph_with(ppt, value = rvg::dml(ggobj = value), location = ph_location_label(content$location)))
    }
    return(ph_with(ppt, value = value, location = ph_location_label(content$location)))
  }
  if (inherits(value, "grob")) {
    figure <- list(grob = value)
    size <- panel_size(panel)
    return(ph_with_img(
      ppt, figure = figure, fig_width = size$width, fig_height = size$height,
      figure_loc = ph_location_label(content$location)
    ))
  }
  stop("Unsupported secondary slide content.", call. = FALSE)
}

#' generate slides based on output
#'
#' @param outputs List of output
#' @param template Template file path
#' @param outfile Out file path
#' @param fig_width figure width in inch
#' @param fig_height figure height in inch
#' @param t_lpp An integer specifying the table lines per page \cr
#'    Specify this optional argument to modify the length of all of the table displays
#' @param t_cpp An integer specifying the table columns per page\cr
#'    Specify this optional argument to modify the width of all of the table displays
#' @param l_lpp An integer specifying the listing lines per page\cr
#'    Specify this optional argument to modify the length of all of the listings display
#' @param l_cpp An integer specifying the listing columns per page\cr
#'    Specify this optional argument to modify the width of all of the listings display
#' @param fig_editable whether we want the figure to be editable in pptx viewers, defaults to FALSE
#' @param font_size Deck-wide default table font sizes, a named `list` with any
#'   of `body`, `header`, `footer` (point sizes). Per-slide sizes declared in the
#'   spec (a `font_size:` block on the entry) override these. Applied by wrapping
#'   the slide's `table_format` via [with_font_sizes()]. The footer defaults to
#'   the body size, or 8 pt when no body size is supplied; see Details.
#' @param extra_content One [slide_content()] descriptor for a second object on
#'   a single-output deck. A spec entry can provide the descriptor instead.
#' @param ... arguments passed to program
#' @return No return value, called for side effects
#' @details
#' ## Per-slide font size
#' Each output carries its spec entry as an attribute (set by
#' [generate_outputs()]). `generate_slides()` reads two optional keys from it:
#' `table_format` (a formatter function) and `font_size` (a named list with
#' `body`/`header`/`footer`). The effective formatter for a slide is
#' `with_font_sizes(table_format, body, header, footer)`, so font sizes can be
#' set per slide directly in the spec, e.g.
#' \preformatted{
#' t_dm_slide_FAS:
#'   program: t_dm_slide
#'   suffix: FAS
#'   table_format: black_format_tb
#'   font_size:
#'     body: 6
#'     header: 6
#'     footer: 5
#' }
#' The `font_size` argument sets deck-wide defaults; per-slide values win.
#' When no footer size is supplied, the resolved body size is used, falling back
#' to 8 pt. This default is applied to the Confidential footnote on every
#' supported slide path, including `decor = FALSE`.
#'
#' ## Multi-content slides
#' A spec entry may define `extra_content` with [slide_content()], or a caller
#' may pass one descriptor through `extra_content` when `outputs` contains one
#' primary object. Both the primary location (`table_loc` or `figure_loc`) and
#' the secondary location must be named placeholders in an explicitly selected
#' layout. Each panel must fit and resolve to exactly one page; pagination and
#' truncation are rejected.
#' @export
#' @examplesIf require(filters)
#'
#' # Example 1. When applying to the whole pipeline
#' library(dplyr)
#' data <- list(
#'   adsl = eg_adsl |> dplyr::mutate(FASFL = SAFFL),
#'   adae = eg_adae
#' )
#'
#'
#' filters::load_filters(
#'   yaml_file = system.file("filters.yml", package = "autoslider.core"),
#'   overwrite = TRUE
#' )
#'
#'
#' spec_file <- system.file("spec.yml", package = "autoslider.core")
#' spec_file |>
#'   read_spec() |>
#'   filter_spec(program %in% c("t_dm_slide")) |>
#'   generate_outputs(datasets = data) |>
#'   decorate_outputs() |>
#'   generate_slides()
#'
#' # Example 2. When applying to an rtable object or an rlisting object
#' adsl <- eg_adsl
#' t_dm_slide(adsl, "TRT01P", c("SEX", "AGE")) |>
#'   generate_slides()
generate_slides <- function(outputs,
                            outfile = paste0(tempdir(), "/output.pptx"),
                            template = file.path(system.file(package = "autoslider.core"), "theme/basic.pptx"),
                            fig_width = 9, fig_height = 5, t_lpp = 20, t_cpp = 200,
                            l_lpp = 20, l_cpp = 150, fig_editable = FALSE,
                            font_size = NULL, extra_content = NULL, ...) {
  if (any(c(
    inherits(outputs, "VTableTree"),
    inherits(outputs, "listing_df")
  ))) {
    if (inherits(outputs, "listing_df")) {
      current_title <- main_title(outputs)
    } else {
      current_title <- outputs@main_title
    }
    outputs <- list(
      decorate(outputs, titles = current_title, footnotes = confidential_footnote)
    )
  } else if (any(c(
    inherits(outputs, "data.frame"),
    inherits(outputs, "ggplot"),
    inherits(outputs, "gtsummary"),
    inherits(outputs, "tbl_roche_summary"),
    inherits(outputs, "dVTableTree"),
    inherits(outputs, "dlisting"),
    inherits(outputs, "grob")
  ))) {
    if (inherits(outputs, "ggplot")) {
      current_title <- outputs$labels$title
      if (is.null(current_title)) {
        current_title <- ""
      }
      outputs <- decorate.ggplot(outputs, titles = current_title)
    } else if (inherits(outputs, "grob")) {
      outputs <- decorate.grob(outputs)
    } else if (
      (inherits(outputs, "gtsummary") || inherits(outputs, "tbl_roche_summary")) &&
      !inherits(outputs, "dgtsummary")
    ) {
      current_title <- tryCatch(outputs$table_styling$caption, error = function(e) NULL)
      if (is.null(current_title) || !nzchar(current_title)) current_title <- ""
      outputs <- decorate(outputs, titles = current_title)
    }

    outputs <- list(outputs)
  }

  assert_that(is.list(outputs))
  if (!is.null(extra_content) && length(outputs) != 1L) {
    stop("`extra_content` can be supplied directly only when generating one primary output.", call. = FALSE)
  }

  # ======== generate slides =======#
  # Arguments forwarded to to_flextable()/table_to_slide(). `table_format` and
  # `font_size` are managed per-slide below, so they are pulled out of the
  # forwarded set to avoid clashing with the values we inject.
  dots <- list(...)
  fwd <- dots
  fwd$table_format <- NULL

  # Deck-wide default font sizes. Accept the tidy `font_size = list(...)` form
  # and, for backwards compatibility, scalar *_font_size passed via `...`.
  deck_fs <- font_size %||% list()
  deck_fs$body <- deck_fs$body %||% dots$body_font_size
  deck_fs$header <- deck_fs$header %||% dots$header_font_size
  deck_fs$footer <- deck_fs$footer %||% dots$footer_font_size
  fwd$body_font_size <- NULL
  fwd$header_font_size <- NULL
  fwd$footer_font_size <- NULL

  # Merge deck defaults with a slide's own spec `font_size` block (slide wins).
  slide_fs <- function(x) {
    sp <- attr(x, "spec")
    modifyList(deck_fs, (sp$font_size) %||% list())
  }
  resolve_font_sizes <- function(x) {
    fs <- slide_fs(x)
    fs$footer <- fs$footer %||% fs$body %||% default_footer_font_size
    fs
  }
  resolve_footer_font_size <- function(x) {
    resolve_font_sizes(x)$footer
  }
  # Effective formatter for a slide: its spec `table_format` (else the deck-wide
  # one, else `default_fmt`), wrapped so the resolved font sizes are applied.
  resolve_format <- function(x, default_fmt) {
    sp <- attr(x, "spec")
    fs <- resolve_font_sizes(x)
    base_fmt <- (sp$table_format) %||% dots$table_format %||% default_fmt
    with_font_sizes(base_fmt, fs$body, fs$header, fs$footer)
  }
  # Arguments that belong to `table_to_slide()` (slide placement / decoration) but
  # are not accepted by `to_flextable()` or its formatter helpers. A spec may set
  # these (e.g. `layout = "08_TitleAndContent"`, `table_loc = ...`); they must reach
  # `call_slide()` but must be dropped before `to_flextable()`, otherwise they leak
  # through the formatter's `...` (e.g. `autoslider_format()`) and error with
  # "unused argument".
  slide_only_args <- c(
    "decor", "layout", "master", "table_loc", "figure_loc", "usernotes",
    "footer_font_size"
  )
  fwd_ft <- fwd[setdiff(names(fwd), slide_only_args)]
  call_ft <- function(x, more) {
    do.call(to_flextable, c(list(x = x), more, fwd_ft))
  }
  call_slide <- function(ppt, content, more) {
    do.call(table_to_slide, c(list(ppt = ppt, content = content), more, fwd))
  }

  resolve_composition <- function(x, kind) {
    sp <- attr(x, "spec") %||% list()
    spec_extra <- sp$extra_content
    if (!is.null(spec_extra) && !is.null(extra_content)) {
      stop("Define `extra_content` either on the spec entry or in `generate_slides()`, not both.", call. = FALSE)
    }
    secondary <- spec_extra %||% extra_content
    if (is.null(secondary)) {
      return(NULL)
    }
    secondary <- validate_slide_content(secondary)

    location_name <- if (identical(kind, "figure")) "figure_loc" else "table_loc"
    primary_label <- sp[[location_name]] %||% dots[[location_name]]
    if (!is.character(primary_label) || length(primary_label) != 1L || is.na(primary_label) || !nzchar(primary_label)) {
      stop(
        sprintf("A multi-content slide requires an explicit named `%s` for the primary panel.", location_name),
        call. = FALSE
      )
    }
    if (identical(primary_label, secondary$location)) {
      stop("Primary and secondary content must use different placeholders.", call. = FALSE)
    }

    layout <- sp$layout %||% dots$layout %||% "Title and Content"
    master <- resolve_slide_master(ppt, layout, sp$master %||% dots$master, require_unique = TRUE)
    primary_panel <- composite_panel_properties(ppt, layout, master, primary_label, "primary")
    secondary_panel <- composite_panel_properties(ppt, layout, master, secondary$location, "secondary")

    list(
      content = secondary,
      layout = layout,
      master = master,
      primary_label = primary_label,
      primary_panel = primary_panel,
      secondary_panel = secondary_panel
    )
  }

  require_one_page <- function(content, output, panel) {
    if (length(content) != 1L) {
      stop(
        sprintf(
          "%s: %s content requires %d pages; a multi-content slide requires one page per panel.",
          output, panel, length(content)
        ),
        call. = FALSE
      )
    }
    content[[1]]
  }

  # set slides layout
  ppt <- read_pptx(path = template)
  location_ <- officer::fortify_location(ph_location_fullsize(), doc = ppt)
  width <- location_$width
  height <- location_$height

  # add content to slides template
  for (x in outputs) {
    if (inherits(x, "dVTableTree") || inherits(x, "VTableTree")) {
      tf <- resolve_format(x, orange_format)
      footer_pt <- resolve_footer_font_size(x)
      y <- call_ft(x, list(lpp = t_lpp, cpp = t_cpp, table_format = tf))
      usernotes <- x@usernotes
      composite <- resolve_composition(x, "table")
      if (!is.null(composite)) {
        tt <- require_one_page(y, (attr(x, "spec") %||% list(output = "primary"))$output, "primary")
        validate_flextable_fits_panel(tt$ft, composite$primary_panel, "Primary")
        ppt <- table_to_slide(
          ppt, tt, table_loc = ph_location_label(composite$primary_label),
          usernotes = usernotes, footer_font_size = footer_pt, layout = composite$layout,
          master = composite$master, extra_content = composite$content,
          secondary_panel = composite$secondary_panel, fig_editable = fig_editable
        )
        next
      }
      for (tt in y) {
        ppt <- call_slide(ppt, tt, list(
          table_loc = center_table_loc(tt$ft, ppt_width = width, ppt_height = height),
          usernotes = usernotes, footer_font_size = footer_pt
        ))
      }
    } else if (inherits(x, "dlisting")) {
      footer_pt <- resolve_footer_font_size(x)
      y <- call_ft(x, list(cpp = l_cpp, lpp = l_lpp))
      composite <- resolve_composition(x, "table")
      if (!is.null(composite)) {
        tt <- require_one_page(y, (attr(x, "spec") %||% list(output = "primary"))$output, "primary")
        validate_flextable_fits_panel(tt$ft, composite$primary_panel, "Primary")
        ppt <- table_to_slide(
          ppt, tt, table_loc = ph_location_label(composite$primary_label),
          footer_font_size = footer_pt, layout = composite$layout, master = composite$master,
          extra_content = composite$content, secondary_panel = composite$secondary_panel,
          fig_editable = fig_editable
        )
        next
      }
      for (tt in y) {
        ppt <- call_slide(ppt, tt, list(
          table_loc = center_table_loc(tt$ft, ppt_width = width, ppt_height = height),
          footer_font_size = footer_pt
        ))
      }
    } else if (inherits(x, "data.frame")) { # this is dedicated for small data frames without pagination
      tf <- resolve_format(x, orange_format)
      footer_pt <- resolve_footer_font_size(x)
      y <- call_ft(x, list(table_format = tf))
      composite <- resolve_composition(x, "table")
      if (!is.null(composite)) {
        validate_flextable_fits_panel(y, composite$primary_panel, "Primary")
        ppt <- table_to_slide(
          ppt, y, decor = FALSE, table_loc = ph_location_label(composite$primary_label),
          footer_font_size = footer_pt, layout = composite$layout, master = composite$master,
          extra_content = composite$content, secondary_panel = composite$secondary_panel,
          fig_editable = fig_editable
        )
        next
      }
      ppt <- call_slide(ppt, y, list(decor = FALSE, footer_font_size = footer_pt))
    } else if (inherits(x, "dgtsummary")) {
      tf <- resolve_format(x, autoslider_format)
      footer_pt <- resolve_footer_font_size(x)
      y <- call_ft(x, list(
        lpp = t_lpp, ppt_height = height, ppt_width = width, table_format = tf
      ))
      composite <- resolve_composition(x, "table")
      if (!is.null(composite)) {
        tt <- require_one_page(y, (attr(x, "spec") %||% list(output = "primary"))$output, "primary")
        validate_flextable_fits_panel(tt$ft, composite$primary_panel, "Primary")
        ppt <- table_to_slide(
          ppt, tt, table_loc = ph_location_label(composite$primary_label),
          footer_font_size = footer_pt, layout = composite$layout, master = composite$master,
          extra_content = composite$content, secondary_panel = composite$secondary_panel,
          fig_editable = fig_editable
        )
        next
      }
      for (tt in y) {
        ppt <- call_slide(ppt, tt, list(
          table_loc = center_gts_table_loc(tt$ft, ppt_width = width, ppt_height = height),
          footer_font_size = footer_pt
        ))
      }
    } else if (inherits(x, "gtsummary") || inherits(x, "tbl_roche_summary")) {
      tf <- resolve_format(x, autoslider_format)
      footer_pt <- resolve_footer_font_size(x)
      y <- call_ft(x, list(table_format = tf))
      composite <- resolve_composition(x, "table")
      if (!is.null(composite)) {
        validate_flextable_fits_panel(y, composite$primary_panel, "Primary")
        ppt <- table_to_slide(
          ppt, y, decor = FALSE, table_loc = ph_location_label(composite$primary_label),
          footer_font_size = footer_pt, layout = composite$layout, master = composite$master,
          extra_content = composite$content, secondary_panel = composite$secondary_panel,
          fig_editable = fig_editable
        )
        next
      }
      ppt <- call_slide(ppt, y, list(decor = FALSE, footer_font_size = footer_pt))
    } else {
      if (any(class(x) %in% c("decoratedGrob", "decoratedGrobSet", "ggplot"))) {
        if (inherits(x, "ggplot")) {
          x <- decorate.ggplot(x)
        }

        assertthat::assert_that(inherits(x, "decoratedGrob") || inherits(x, "decoratedGrobSet"))

        composite <- resolve_composition(x, "figure")
        if (!is.null(composite)) {
          primary_size <- panel_size(composite$primary_panel)
          ppt <- figure_to_slide(
            ppt, content = x, fig_width = primary_size$width, fig_height = primary_size$height,
            figure_loc = ph_location_label(composite$primary_label), fig_editable = fig_editable,
            layout = composite$layout, master = composite$master,
            extra_content = composite$content, secondary_panel = composite$secondary_panel
          )
          next
        }

        ppt <- figure_to_slide(ppt,
          content = x, fig_width = fig_width, fig_height = fig_height,
          figure_loc = center_figure_loc(fig_width, fig_height, ppt_width = width, ppt_height = 1.17 * height),
          fig_editable = fig_editable, ...
        )
      } else {
        if (inherits(x, "autoslider_error")) {
          message(x)
        } else {
          next
        }
      }
    }
  }
  print(ppt, target = outfile)
}

#' Generate flextable for preview first page
#'
#' @param x rtables or data.frame
#' @return A flextable or a ggplot object depending to the input.
#' @export
#' @examples
#' # Example 1. preview table
#' library(dplyr)
#' adsl <- eg_adsl
#' t_dm_slide(adsl, "TRT01P", c("SEX", "AGE")) |> slides_preview()
slides_preview <- function(x) {
  if (inherits(x, "VTableTree")) {
    ret <- to_flextable(paginate_table(x, lpp = 20)[[1]])
  } else if (inherits(x, "listing_df")) {
    new_colwidth <- formatters::propose_column_widths(x)
    ret <- to_flextable(old_paginate_listing(x, cpp = 150, lpp = 20)[[1]],
      col_width = new_colwidth
    )
  } else if (inherits(x, "ggplot")) {
    ret <- x
  } else {
    stop("Unintended usage!")
  }
  ret
}

get_body_bottom_location <- function(ppt) {
  location_ <- officer::fortify_location(ph_location_fullsize(), doc = ppt)
  width <- location_$width
  height <- location_$height
  top <- 0.7 * height
  left <- 0.1 * width
  ph <- ph_location(left = left, top = top)
  ph
}


#' create location container to center the table
#'
#' @param ft Flextable object
#' @param ppt_width Powerpoint width
#' @param ppt_height Powerpoint height
#' @return Location for a placeholder
center_table_loc <- function(ft, ppt_width, ppt_height) {
  top <- (ppt_height - sum(dim(ft)$heights)) / 2
  left <- (ppt_width - sum(dim(ft)$widths)) / 2
  ph <- ph_location(left = left, top = top)
  ph
}

# Position table in the body area below the title placeholder.
# title_bottom marks where the title area ends; the table is centered
# in the remaining vertical space so it never overlaps the title or
# extends past the slide bottom.
center_gts_table_loc <- function(ft, ppt_width, ppt_height, title_bottom = 1.5) {
  ft_height <- flextable_dim(ft)$heights
  ft_width <- flextable_dim(ft)$widths
  body_height <- ppt_height - title_bottom
  top <- title_bottom + max(0, (body_height - ft_height) / 2)
  left <- max(0, (ppt_width - ft_width) / 2)
  ph_location(left = left, top = top, width = ft_width, height = ft_height)
}

#' Adjust title line break and font size
#'
#' @param title Character string
#' @param max_char Integer specifying the maximum number of characters in one line
#' @param title_color Title color
get_proper_title <- function(title, max_char = 60, title_color = "#1C2B39") {
  # cat(nchar(title), " ", as.integer(24-nchar(title)/para), "\n")
  title <- gsub("\\n", "\\s", title)
  new_title <- ""

  while (nchar(title) > max_char) {
    spaces <- gregexpr("\\s", title)
    new_title <- paste0(new_title, "\n", substring(title, 1, max(spaces[[1]][spaces[[1]] <= max_char])))
    title <- substring(title, max(spaces[[1]][spaces[[1]] <= max_char]) + 1, nchar(title))
  }

  new_title <- paste0(new_title, "\n", title)

  ftext(
    trimws(new_title),
    fp_text(
      font.size = floor(26 - nchar(title) / max_char),
      color = title_color
    )
  )
}

#' Add decorated flextable to slides
#'
#' @param ppt Slide
#' @param content Content to be added
#' @param table_loc Table location
#' @param usernotes User notes
#' @param decor Should table be decorated
#' @param layout layout from theme
#' @param footer_font_size Point size for the footnote text, defaulting to 8.
#'   `NULL` keeps the existing footnote size set on the flextable.
#' @param ... additional arguments
#' @return Slide with added content
table_to_slide <- function(ppt, content, decor = TRUE, layout = "Title and Content",
                           table_loc = ph_location_type("body"), usernotes = "",
                           footer_font_size = 8L, master = NULL,
                           extra_content = NULL, secondary_panel = NULL,
                           fig_editable = FALSE, ...) {
  ppt_master <- resolve_slide_master(ppt, layout, master)
  args <- list(...)
  ppt <- layout_default(ppt, layout)

  if (decor) {
    print(content$header)
    out <- content$ft

    if (length(content$footnotes) > 1) {
      content$footnotes <- paste(content$footnotes, collapse = "\n")
    }
    # print(content_footnotes)
    if (content$footnotes != "") {
      footnote_value <- make_footnote_value(content$footnotes, footer_font_size)
      out <- footnote(out,
        i = 1, j = 1,
        value = footnote_value,
        ref_symbols = " ", part = "header", inline = TRUE
      )
    }

    args$arg_header <- list(
      value = fpar(get_proper_title(content$header)),
      location = ph_location_type("title")
    )
  } else {
    out <- content
    out <- footnote(out,
      i = 1, j = 1,
      value = make_footnote_value(confidential_footnote, footer_font_size),
      ref_symbols = " ", part = "header", inline = TRUE
    )
  }

  ppt <- add_slide(ppt, layout = layout, master = ppt_master)
  ppt <- ph_with(ppt, value = out, location = table_loc)
  ppt <- set_notes(ppt, value = usernotes,
                   location = notes_location_type("body"))
  ppt <- add_located_content(ppt, args)

  if (!is.null(extra_content)) {
    if (is.null(secondary_panel)) {
      stop("Secondary panel metadata is required for multi-content slides.", call. = FALSE)
    }
    ppt <- insert_slide_content(
      ppt, validate_slide_content(extra_content), secondary_panel,
      fig_editable = extra_content$fig_editable %||% fig_editable
    )
  }

  ppt
}

add_located_content <- function(ppt, args) {
  ph_with_args <- args[vapply(args, function(x) {
    is.list(x) && all(c("location", "value") %in% names(x))
  }, logical(1))]
  for (arg in ph_with_args) {
    ppt <- ph_with(ppt, value = arg$value, location = arg$location)
  }
  ppt
}

#' Create location container to center the figure, based on ppt size and
#' user specified figure size
#'
#' @param fig_width Figure width
#' @param fig_height Figure height
#' @param ppt_width Slide width
#' @param ppt_height Slide height
#'
#' @return Location for a placeholder from scratch
center_figure_loc <- function(fig_width, fig_height, ppt_width, ppt_height) {
  # center figure
  top <- (ppt_height - fig_height) / 2
  left <- (ppt_width - fig_width) / 2
  ph_location(top = top, left = left)
}

#' Placeholder for ph_with_img
#'
#' @param ppt power point file
#' @param figure image object
#' @param fig_width width of figure
#' @param fig_height height of figure
#' @param figure_loc location of figure
#' @return Location for a placeholder
#' @export
ph_with_img <- function(ppt, figure, fig_width, fig_height, figure_loc) {
  file_name <- tempfile(fileext = ".svg")
  svg(filename = file_name, width = fig_width, height = fig_height, onefile = TRUE)
  grid.draw(figure$grob)
  dev.off()
  on.exit(unlink(file_name))
  ext_img <- external_img(file_name, width = fig_width, height = fig_height)

  ppt |> ph_with(value = ext_img, location = figure_loc, use_loc_size = FALSE)
}

#' Add figure to slides
#'
#' @param ppt slide page
#' @param content content to be added
#' @param decor should decoration be added
#' @param fig_width user specified figure width
#' @param fig_height user specified figure height
#' @param figure_loc location of the figure. Defaults to `ph_location_type("body")`
#' @param layout theme layout
#' @param fig_editable whether we want the figure to be editable in pptx viewers
#' @param ... arguments passed to program
#'
#' @return slide with the added content
figure_to_slide <- function(ppt, content,
                            decor = TRUE,
                            fig_width,
                            fig_height,
                            layout = "Title and Content",
                            figure_loc = ph_location_type("body"),
                            fig_editable = FALSE,
                            master = NULL, extra_content = NULL,
                            secondary_panel = NULL, ...) {
  ppt_master <- resolve_slide_master(ppt, layout, master)
  ppt <- layout_default(ppt, layout)
  args <- list(...)


  if (decor) {
    args$arg_header <- list(
      value = fpar(get_proper_title(content$titles)),
      location = ph_location_type("title")
    )
  }

  if ("decoratedGrob" %in% class(content)) {
    ppt <- add_slide(ppt, layout = layout, master = ppt_master)
    if (fig_editable) {
      content_list <- g_export(content)
      ppt <- ph_with(ppt, content_list$dml, location = figure_loc)
    } else {
      ppt <- ph_with_img(ppt, content, fig_width, fig_height, figure_loc)
    }

    ppt <- add_located_content(ppt, args)
    if (!is.null(extra_content)) {
      if (is.null(secondary_panel)) {
        stop("Secondary panel metadata is required for multi-content slides.", call. = FALSE)
      }
      ppt <- insert_slide_content(
        ppt, validate_slide_content(extra_content), secondary_panel,
        fig_editable = extra_content$fig_editable %||% fig_editable
      )
    }
    ppt
  } else if ("decoratedGrobSet" %in% class(content)) { # for decoratedGrobSet, a list of figures are created and added
    if (!is.null(extra_content)) {
      stop("A decoratedGrobSet cannot be used as the primary panel of a multi-content slide.", call. = FALSE)
    }
    # revisit, to make more efficent
    for (figure in content) {
      ppt <- add_slide(ppt, layout = layout, master = ppt_master)
      ppt <- ph_with_img(ppt, figure, fig_width, fig_height, figure_loc)
    }
    ppt
  } else {
    stop("Should not reach here")
  }
}
