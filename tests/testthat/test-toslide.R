test_that("Test layout is in layout", {
  adsl <- eg_adsl
  out1 <- t_dm_slide(adsl, "TRT01P", c("SEX", "AGE", "RACE", "ETHNIC", "COUNTRY"))
  expect_no_error(generate_slides(out1, paste0(tempdir(), "/dm.pptx")))
  expect_error(generate_slides(out1, paste0(tempdir(), "/dm.pptx"), layout = "some layout"))
})

test_that("multi-content slides place two prepared tables on one slide", {
  primary <- data.frame(panel = "Primary", value = 1)
  secondary <- flextable::flextable(data.frame(panel = "Secondary", value = 2))
  outfile <- withr::local_tempfile(fileext = ".pptx")
  template <- testthat::test_path("..", "..", "inst", "theme", "basic.pptx")

  generate_slides(
    primary,
    outfile = outfile,
    template = template,
    layout = "Two Content",
    table_loc = "Content Placeholder 2",
    col_width = c(1, 1),
    extra_content = slide_content(secondary, "Content Placeholder 3")
  )

  ppt <- officer::read_pptx(outfile)
  expect_length(ppt, 1L)
  slide_text <- officer::slide_summary(ppt)$text
  expect_true(any(grepl("Primary", slide_text, fixed = TRUE)))
  expect_true(any(grepl("Secondary", slide_text, fixed = TRUE)))
})

test_that("multi-content slides require distinct named primary locations", {
  primary <- data.frame(panel = "Primary", value = 1)
  secondary <- flextable::flextable(data.frame(panel = "Secondary", value = 2))
  template <- testthat::test_path("..", "..", "inst", "theme", "basic.pptx")

  expect_error(
    generate_slides(
      primary,
      outfile = withr::local_tempfile(fileext = ".pptx"),
      template = template,
      layout = "Two Content",
      col_width = c(1, 1),
      extra_content = slide_content(secondary, "Content Placeholder 3")
    ),
    "explicit named `table_loc`"
  )
})

test_that("multi-content settings can be attached to the primary spec", {
  primary <- data.frame(panel = "Primary from spec", value = 1)
  secondary <- flextable::flextable(data.frame(panel = "Secondary from spec", value = 2))
  attr(primary, "spec") <- list(
    output = "primary_from_spec",
    layout = "Two Content",
    table_loc = "Content Placeholder 2",
    extra_content = slide_content(secondary, "Content Placeholder 3")
  )
  outfile <- withr::local_tempfile(fileext = ".pptx")
  template <- testthat::test_path("..", "..", "inst", "theme", "basic.pptx")

  generate_slides(
    primary,
    outfile = outfile,
    template = template,
    col_width = c(1, 1)
  )

  ppt <- officer::read_pptx(outfile)
  expect_length(ppt, 1L)
  slide_text <- officer::slide_summary(ppt)$text
  expect_true(any(grepl("Primary from spec", slide_text, fixed = TRUE)))
  expect_true(any(grepl("Secondary from spec", slide_text, fixed = TRUE)))
})

test_that("secondary table conversion rejects pagination", {
  ft <- flextable::flextable(data.frame(value = 1))
  pages <- structure(list(list(ft = ft), list(ft = ft)), class = "dflextable")

  expect_error(
    as_slide_flextable(pages),
    "requires more than one page"
  )
})
