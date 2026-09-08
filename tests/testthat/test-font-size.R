test_that("with_font_sizes returns the formatter unchanged when no sizes given", {
  expect_identical(with_font_sizes(black_format_tb), black_format_tb)
})

test_that("with_font_sizes forwards sizes a formatter with explicit args declares", {
  seen <- list()
  rec_fmt <- function(ft, body_font_size = NA, header_font_size = NA,
                      footer_font_size = NA, ...) {
    seen <<- list(body = body_font_size, header = header_font_size, footer = footer_font_size)
    ft
  }
  ft <- flextable::flextable(head(iris))
  out <- with_font_sizes(rec_fmt, body_font_size = 7, header_font_size = 6, footer_font_size = 5)(ft)

  expect_s3_class(out, "flextable")
  expect_equal(seen, list(body = 7, header = 6, footer = 5))
})

test_that("with_font_sizes drops sizes a formatter (no dots) cannot accept", {
  # rec_fmt only knows body_font_size and has no `...`: footer must be dropped,
  # not passed through (which would raise an 'unused argument' error).
  seen <- NA
  rec_fmt <- function(ft, body_font_size = NA) {
    seen <<- body_font_size
    ft
  }
  ft <- flextable::flextable(head(iris))
  expect_silent(
    out <- with_font_sizes(rec_fmt, body_font_size = 8, footer_font_size = 5)(ft)
  )
  expect_s3_class(out, "flextable")
  expect_equal(seen, 8)
})

test_that("with_font_sizes only injects the sizes that are supplied", {
  seen <- list()
  rec_fmt <- function(ft, body_font_size = "unset", footer_font_size = "unset", ...) {
    seen <<- list(body = body_font_size, footer = footer_font_size)
    ft
  }
  ft <- flextable::flextable(head(iris))
  with_font_sizes(rec_fmt, body_font_size = 9)(ft)
  expect_equal(seen$body, 9)
  expect_identical(seen$footer, "unset") # left to the formatter's own default
})

test_that("built-in formatters accept footer_font_size", {
  ft <- flextable::flextable(head(iris))
  expect_s3_class(autoslider_format(ft, footer_font_size = 6), "flextable")
  expect_s3_class(black_format_tb(ft, footer_font_size = 6), "flextable")
})

test_that("generate_slides honours a per-slide font_size block from the spec", {
  skip_if_not_installed("filters")

  out <- t_dm_slide(adsl, "TRT01A", c("SEX", "AGE")) %>%
    decorate(titles = "Demographics", footnotes = "footnote")
  # emulate what generate_outputs()/decorate_outputs() attach
  attr(out, "spec") <- list(
    table_format = black_format_tb,
    font_size = list(body = 6, header = 6, footer = 5)
  )

  outfile <- withr::local_tempfile(fileext = ".pptx")
  expect_no_error(generate_slides(list(out), outfile = outfile))
  expect_true(file.exists(outfile))
})

test_that("generate_slides deck-wide font_size default works without a spec", {
  skip_if_not_installed("filters")

  out <- t_dm_slide(adsl, "TRT01A", c("SEX", "AGE")) %>%
    decorate(titles = "Demographics", footnotes = "footnote")

  outfile <- withr::local_tempfile(fileext = ".pptx")
  expect_no_error(
    generate_slides(
      list(out),
      outfile = outfile,
      table_format = black_format_tb,
      font_size = list(body = 7, header = 7)
    )
  )
  expect_true(file.exists(outfile))
})
