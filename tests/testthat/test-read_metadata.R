test_that("read_metadata reads a yaml file into a named list of token values", {
  metadata_file <- system.file("metadata.yml", package = "autoslider.core")
  metadata <- read_metadata(metadata_file)

  expect_type(metadata, "list")
  expect_true(checkmate::test_named(metadata))
  expect_equal(metadata$study, "BP12345")
})

test_that("read_metadata errors when the file does not exist", {
  expect_error(read_metadata(tempfile(fileext = ".yml")), "exist")
})

test_that("read_spec accepts a metadata file path and substitutes the tokens", {
  spec_file <- system.file("spec.yml", package = "autoslider.core")
  metadata_file <- withr::local_tempfile(fileext = ".yml")
  writeLines("study: BP12345", metadata_file)

  from_file <- read_spec(spec_file, metadata = metadata_file)
  from_list <- read_spec(spec_file, metadata = list(study = "BP12345"))

  # Reading the path is equivalent to passing the list directly.
  expect_equal(from_file[[1]]$study, "BP12345")
  expect_equal(lapply(from_file, `[[`, "study"), lapply(from_list, `[[`, "study"))
})

test_that("metadata read from a file flows into decorated titles", {
  skip_if_not_installed("rtables")
  metadata_file <- withr::local_tempfile(fileext = ".yml")
  writeLines("study: BP12345", metadata_file)

  tbl <- rtables::basic_table() |>
    rtables::split_cols_by("ARM") |>
    rtables::analyze("AGE") |>
    rtables::build_table(data.frame(ARM = c("A", "B"), AGE = c(30, 40)))

  dec <- decorate(tbl, titles = "Study {study}", metadata = read_metadata(metadata_file))
  expect_true(any(grepl("BP12345", dec@titles)))
})
