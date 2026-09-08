test_that("apply_tokens substitutes tokens from a metadata list", {
  expect_equal(
    as.character(apply_tokens("Study {study} demographics", list(study = "BP12345"))),
    "Study BP12345 demographics"
  )
})

test_that("apply_tokens collapses a character vector by newline (matching glue)", {
  expect_equal(
    as.character(apply_tokens(c("Line 1 {study}", "Line 2"), list(study = "BP12345"))),
    "Line 1 BP12345\nLine 2"
  )
})

test_that("apply_tokens is a no-op when there are no tokens", {
  expect_equal(as.character(apply_tokens("No tokens here", list(study = "BP12345"))), "No tokens here")
  expect_equal(as.character(apply_tokens("No tokens here")), "No tokens here")
})

test_that("apply_tokens returns the input unchanged for zero-length input", {
  expect_equal(apply_tokens(character(0), list(study = "BP12345")), character(0))
})

test_that("apply_tokens errors informatively on an unknown token", {
  expect_error(
    apply_tokens("Study {missing}", list(study = "BP12345")),
    "Failed to substitute metadata tokens"
  )
})

test_that("apply_tokens validates its arguments", {
  expect_error(apply_tokens(42L, list(study = "BP12345")), "character")
  expect_error(apply_tokens("Study {study}", metadata = "not-a-list"), "list")
})

test_that("apply_tokens falls back to the calling environment when metadata lacks the token", {
  study <- "ENV123"
  expect_equal(as.character(apply_tokens("Study {study}")), "Study ENV123")
})

test_that("metadata tokens flow into decorated table titles", {
  skip_if_not_installed("rtables")
  tbl <- rtables::basic_table() |>
    rtables::split_cols_by("ARM") |>
    rtables::analyze("AGE") |>
    rtables::build_table(data.frame(ARM = c("A", "B"), AGE = c(30, 40)))

  dec <- decorate(tbl, titles = "Study {study}", metadata = list(study = "BP12345"))
  expect_true(any(grepl("BP12345", dec@titles)))
})
