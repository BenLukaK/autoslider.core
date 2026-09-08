#' Table color and font
#'
#' @description Zebra themed color
#'
#' @name autoslider_format
NULL

#' @describeIn autoslider_format
#'
#' User defined color code and font size
#'
#' @param ft flextable object
#' @param odd_header Hex color code, default to deep sky blue
#' @param odd_body Hex color code, default to alice blue
#' @param even_header Hex color code, default to slate gray
#' @param even_body Hex color code, default to slate gray
#' @param font_name Font name, default to arial
#' @param body_font_size Font size of the table content, default to 12
#' @param header_font_size Font size of the table header, default to 14
#' @return A flextable with applied theme.
#' @export
autoslider_format <- function(ft,
                              odd_header = "#0EAED5", # "deepskyblue2",
                              odd_body = "#EBF5FA", # "aliceblue",
                              even_header = "#0EAED5", # "slategray1",
                              even_body = "#D0E4F2", # "slategray1" # slategray1,
                              font_name = "arial",
                              body_font_size = 12,
                              header_font_size = 14,
                              footer_font_size = body_font_size) {
  ft %>%
    theme_zebra(
      odd_header = odd_header,
      odd_body = odd_body,
      even_header = odd_header,
      even_body = even_body
    ) %>%
    font(fontname = font_name, part = "all") %>%
    fontsize(size = body_font_size, part = "body") %>%
    color(color = "white", part = "header") %>%
    fontsize(size = header_font_size, part = "header") %>%
    fontsize(size = footer_font_size, part = "footer") %>%
    bold(part = "header")
}


#' @describeIn autoslider_format
#'
#' Blue color theme
#'
#' @param ft flextable object
#' @param ... arguments passed to program
#'
#' @export
blue_format <- function(ft, ...) {
  ft %>% autoslider_format(
    odd_header = "#0B41CD",
    odd_body = "#1482FA",
    even_body = "#BDE3FF",
    ...
  )
}

#' @describeIn autoslider_format
#'
#' Orange color theme
#'
#' @param ft flextable object
#' @param ... arguments passed to program
#'
#' @export
orange_format <- function(ft, ...) {
  ft %>% autoslider_format(
    odd_header = "#ED4A0D",
    odd_body = "#FF7D29",
    even_body = "#FFBD69",
    ...
  )
}

#' @describeIn autoslider_format
#'
#' Red color theme
#'
#' @param ft flextable object
#' @param ... arguments passed to program
#'
#' @export
red_format <- function(ft, ...) {
  ft %>% autoslider_format(
    odd_header = "#C40000",
    odd_body = "#FF1F26",
    even_body = "#FF8782",
    ...
  )
}


#' @describeIn autoslider_format
#'
#' Purple color theme
#'
#' @param ft flextable object
#' @param ... arguments passed to program
#'
#' @export
purple_format <- function(ft, ...) {
  ft %>% autoslider_format(
    odd_header = "#BC36F0",
    odd_body = "#E085FC",
    even_body = "#F2D4FF",
    ...
  )
}

#' @describeIn autoslider_format
#'
#' `AutoslideR` dose formats
#'
#' @param ft flextable object
#' @param header_vals Header
#'
#' @export
autoslider_dose_format <- function(ft, header_vals = names(ft$body$dataset)) {
  # The original implementation used delete_rows and add_header_row, which can be
  # brittle. Using set_header_labels is the idiomatic and more robust way
  # to simply change the text of the header row. This avoids the colwidths error.
  ft %>%
    theme_booktabs() %>%
    # set_header_labels(values = header_vals) %>%
    bold(part = "header") %>%
    border_remove()
}


#' @describeIn autoslider_format
#'
#' Black color theme
#' @author Nina Qi and Jasmina Uzunovic
#' @param ft flextable object
#' @param ... arguments passed to program
#'
#' @export
black_format_tb <- function(ft, body_font_size = 8, header_font_size = 8,
                            footer_font_size = body_font_size, ...) {
  ft %>%
    theme_booktabs() %>%
    fontsize(size = body_font_size, part = "body") %>%
    fontsize(size = header_font_size, part = "header") %>%
    fontsize(size = footer_font_size, part = "footer") %>%
    bold(part = "header") %>%
    color(color = "blue", part = "header") %>%
    border_inner_h(part = "all", border = fp_border(color = "black")) %>%
    hline_top(part = "all", border = fp_border(color = "black", width = 2)) %>%
    hline_bottom(part = "all", border = fp_border(color = "black", width = 2))
}

#' Wrap a table formatter so it applies fixed font sizes
#'
#' @description
#' Returns a new table-format function that behaves like `table_format` but with
#' the supplied font sizes injected. This is the generic mechanism used by
#' [generate_slides()] to honor per-slide `font_size` settings declared in the
#' spec: a single `table_format` symbol can carry body/header/footer sizes.
#'
#' Sizes are only forwarded to arguments the underlying formatter actually
#' declares. A formatter with an explicit `body_font_size`/`header_font_size`/
#' `footer_font_size` argument (or a `...`) receives the corresponding size;
#' sizes a formatter cannot accept are dropped rather than raising an
#' "unused argument" error. When no sizes are supplied the original formatter is
#' returned unchanged.
#'
#' @param table_format A function taking a flextable as its first argument and
#'   returning a flextable (e.g. [autoslider_format()], [black_format_tb()]).
#' @param body_font_size,header_font_size,footer_font_size Point sizes. `NULL`
#'   (the default) leaves that part to the underlying formatter default.
#'
#' @return A function `function(ft, ...)` returning a styled flextable.
#' @export
#'
#' @examples
#' small <- with_font_sizes(black_format_tb, body_font_size = 6, header_font_size = 6)
#' ft <- flextable::flextable(head(mtcars))
#' small(ft)
with_font_sizes <- function(table_format,
                            body_font_size = NULL,
                            header_font_size = NULL,
                            footer_font_size = NULL) {
  force(table_format)
  sizes <- Filter(Negate(is.null), list(
    body_font_size = body_font_size,
    header_font_size = header_font_size,
    footer_font_size = footer_font_size
  ))
  if (length(sizes) == 0) {
    return(table_format)
  }

  function(ft, ...) {
    fmls <- formalArgs(table_format)
    keep <- if (is.null(fmls) || "..." %in% fmls) {
      sizes
    } else {
      sizes[names(sizes) %in% fmls]
    }
    do.call(table_format, c(list(ft = ft), keep))
  }
}
