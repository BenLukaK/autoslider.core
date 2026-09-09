#' Substitute metadata tokens in text
#'
#' Replaces `glue`-style `{token}` placeholders in a character vector with values
#' taken from a user-controlled `metadata` list. This lets study-level information
#' (e.g. study number, data cut-off date) be injected into slide titles, footnotes
#' and placeholder slides without editing the deck or the spec text directly.
#'
#' Token values are looked up first in `metadata`, then in the calling environment
#' (preserving the existing `glue` behavior used across `decorate` methods).
#' A `{token}` that resolves to neither raises an informative error, so typos are
#' caught early rather than silently producing a broken slide.
#'
#' @param text `character` vector possibly containing `{token}` placeholders.
#' @param metadata Named `list` (or `NULL`) supplying token values. Typically the
#'   spec entry produced by [read_spec()], into which the `metadata` argument has
#'   already been merged.
#'
#' @return A length-one `character` (`glue`) value with tokens substituted and the
#'   input collapsed by newlines, matching the previous `glue::glue()` behavior.
#'
#' @export
#'
#' @examples
#' apply_tokens("Demographics - Study {study}", list(study = "BP12345"))
#' apply_tokens(c("Line 1 {study}", "Line 2"), list(study = "BP12345"))
apply_tokens <- function(text, metadata = NULL) {
  checkmate::assert_character(text, null.ok = FALSE)
  checkmate::assert_list(metadata, null.ok = TRUE)

  if (length(text) == 0) {
    return(text)
  }

  data <- if (is.null(metadata)) list() else as.list(metadata)
  template <- paste(text, collapse = "\n")

  tryCatch(
    glue::glue_data(.x = data, template, .envir = parent.frame()),
    error = function(e) {
      stop(
        "Failed to substitute metadata tokens in text: ", template, "\n",
        "Ensure every {token} has a matching name in `metadata` ",
        "(or the calling environment). Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}
