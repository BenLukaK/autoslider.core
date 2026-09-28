mh_fixture <- function() {
  adsl <- data.frame(
    USUBJID = sprintf("P%02d", 1:6),
    TRT01A = factor(
      c("Drug", "Drug", "Drug", "Placebo", "Placebo", "No events"),
      levels = c("Drug", "Placebo", "No events")
    ),
    stringsAsFactors = FALSE
  )

  admh <- data.frame(
    USUBJID = c("P01", "P01", "P01", "P02", "P02", "P02", "P04", "P05", "OUTSIDE"),
    MHBODSYS = c(
      "Cardiac disorders", "Cardiac disorders", "Cardiac disorders",
      "Cardiac disorders", "Nervous system disorders", "Nervous system disorders",
      "Cardiac disorders", "Nervous system disorders", "Cardiac disorders"
    ),
    MHDECOD = c(
      "Tachycardia", "Tachycardia", "Bradycardia", "Tachycardia",
      "Headache", "Headache", "Bradycardia", NA, "Tachycardia"
    ),
    stringsAsFactors = FALSE
  )

  list(adsl = adsl, admh = admh)
}

mh_matrix <- function(x) {
  formatters::matrix_form(x, indent_rownames = TRUE)$strings
}

test_that("t_mh_slide uses the ADSL population and produces hand-checkable MHT01 counts", {
  dat <- mh_fixture()
  result <- t_mh_slide(dat$adsl, dat$admh)
  cells <- rtables::cell_values(result)
  counts <- function(row) vapply(cells[[row]], function(x) unname(x[1L]), numeric(1L))
  percentages <- function(row) vapply(cells[[row]], function(x) unname(x[2L]), numeric(1L))
  patients <- "Total number of patients with at least one condition"
  conditions <- "Total number of conditions"

  expect_s4_class(result, "VTableTree")
  expect_identical(rtables::main_title(result), "Medical History")

  # ADSL contributes denominators 3, 2, 1, and 6. The OUTSIDE record is excluded.
  expect_equal(rtables::col_counts(result), c(3, 2, 1, 6))
  expected <- list(
    c(2, 2, 0, 4), c(6, 2, 0, 8), c(2, 1, 0, 3), c(4, 1, 0, 5),
    c(1, 1, 0, 2), c(2, 0, 0, 2), c(1, 1, 0, 2), c(2, 1, 0, 3),
    c(0, 1, 0, 1), c(1, 0, 0, 1)
  )
  expect_equal(unname(lapply(names(cells), function(row) unname(counts(row)))), expected)
  expect_equal(unname(percentages(paste("USUBJID", patients, sep = "."))), c(2 / 3, 1, 0, 4 / 6))
  expect_equal(unname(percentages(paste("MHBODSYS.Cardiac disorders", patients, sep = "."))),
               c(2 / 3, 1 / 2, 0, 3 / 6))
  expect_equal(unname(counts(paste("MHBODSYS.Nervous system disorders", conditions, sep = "."))),
               c(2, 1, 0, 3))
})

test_that("t_mh_slide ordering is deterministic and independent of the overall column", {
  dat <- mh_fixture()

  with_total <- trimws(mh_matrix(t_mh_slide(dat$adsl, dat$admh, add_all_patients_col = TRUE))[, 1L])
  without_total <- trimws(mh_matrix(t_mh_slide(dat$adsl, dat$admh, add_all_patients_col = FALSE))[, 1L])
  relevant <- function(x) {
    x[x %in% c(
      "Cardiac disorders", "Nervous system disorders", "Bradycardia", "Tachycardia", "Headache", "<Missing>"
    )]
  }

  expect_identical(relevant(with_total), relevant(without_total))
  expect_lt(match("Cardiac disorders", with_total), match("Nervous system disorders", with_total))
  expect_lt(match("Bradycardia", with_total), match("Tachycardia", with_total))
  expect_false(any(grepl("All Patients", mh_matrix(
    t_mh_slide(dat$adsl, dat$admh, add_all_patients_col = FALSE)
  ), fixed = TRUE)))
})

test_that("t_mh_slide supports a custom arm and retains an arm with no events", {
  dat <- mh_fixture()
  names(dat$adsl)[names(dat$adsl) == "TRT01A"] <- "ACTUAL_ARM"

  result <- t_mh_slide(dat$adsl, dat$admh, arm = "ACTUAL_ARM")
  text <- apply(mh_matrix(result), 1L, paste, collapse = " | ")

  expect_equal(rtables::col_counts(result), c(3, 2, 1, 6))
  expect_true(any(grepl("No events", text)))
})

test_that("t_mh_slide normalizes missing SOC and PT terms", {
  dat <- mh_fixture()
  dat$admh <- rbind(
    dat$admh,
    data.frame(USUBJID = "P03", MHBODSYS = NA_character_, MHDECOD = NA_character_)
  )

  expect_gte(sum(trimws(mh_matrix(t_mh_slide(dat$adsl, dat$admh))) == "<Missing>"), 2L)
})

test_that("t_mh_slide validates identifiers, columns, arms, and arguments", {
  dat <- mh_fixture()

  expect_error(t_mh_slide(transform(dat$adsl, USUBJID = replace(USUBJID, 1L, NA)), dat$admh), "USUBJID")
  expect_error(t_mh_slide(rbind(dat$adsl, dat$adsl[1L, ]), dat$admh), "USUBJID|unique|duplicate")
  expect_error(t_mh_slide(dat$adsl[, -1L, drop = FALSE], dat$admh), "USUBJID")
  expect_error(t_mh_slide(dat$adsl, dat$admh[, -2L]), "MHBODSYS")
  expect_error(t_mh_slide(dat$adsl, dat$admh[, -3L]), "MHDECOD")
  expect_error(t_mh_slide(dat$adsl, dat$admh, arm = "UNKNOWN"), "UNKNOWN|arm")
  expect_error(t_mh_slide(dat$adsl, dat$admh, add_all_patients_col = NA), "add_all_patients_col|flag|TRUE|FALSE")

  admh_with_bad_arm <- transform(dat$admh, TRT01A = "Drug")
  admh_with_bad_arm$TRT01A[admh_with_bad_arm$USUBJID == "P04"] <- "Drug"
  expect_error(t_mh_slide(dat$adsl, admh_with_bad_arm), "must agree")
})

test_that("t_mh_slide returns a titled null report when no MH records are in the population", {
  dat <- mh_fixture()
  empty <- dat$admh[0L, ]
  result <- t_mh_slide(dat$adsl, empty)

  expect_s4_class(result, "VTableTree")
  expect_identical(rtables::main_title(result), "Medical History")
  expect_true(any(grepl("Null Report", mh_matrix(result), fixed = TRUE)))

  outside_only <- transform(dat$admh[1L, ], USUBJID = "OUTSIDE")
  expect_true(any(grepl("Null Report", mh_matrix(t_mh_slide(dat$adsl, outside_only)), fixed = TRUE)))
})

test_that("t_mh_slide converts through the package VTableTree formatter", {
  dat <- mh_fixture()
  result <- t_mh_slide(dat$adsl, dat$admh)

  ft <- to_flextable(result, table_format = autoslider_format)
  expect_s3_class(ft, "flextable")
  expect_gt(nrow(ft$body$dataset), 0L)
  expect_true(any(grepl("Cardiac disorders", unlist(ft$body$dataset), fixed = TRUE)))
})

test_that("bundled MH example is stable", {
  expect_snapshot(t_mh_slide(eg_adsl, eg_admh))
})

test_that("bundled MH spec runs through output decoration and PowerPoint generation", {
  testthat::skip_if_not_installed("filters")
  filters::load_filters(system.file("filters.yml", package = "autoslider.core"), overwrite = TRUE)
  spec <- read_spec(system.file("spec.yml", package = "autoslider.core"))
  mh_spec <- filter_spec(spec, program == "t_mh_slide")
  expect_length(mh_spec, 1L)

  outputs <- generate_outputs(mh_spec, datasets = list(adsl = eg_adsl, admh = eg_admh), verbose_level = 0L)
  expect_false(any(vapply(outputs, inherits, logical(1), "autoslider_error")))
  decorated <- decorate_outputs(outputs, version_label = NULL, for_test = TRUE)
  expect_false(any(vapply(decorated, inherits, logical(1), "autoslider_error")))

  outfile <- tempfile(fileext = ".pptx")
  expect_no_error(generate_slides(decorated, outfile = outfile))
  ppt <- officer::read_pptx(outfile)
  expect_gt(length(ppt), 0L)
  extracted <- tempfile("mh-pptx-")
  utils::unzip(outfile, exdir = extracted)
  slides <- list.files(file.path(extracted, "ppt", "slides"), pattern = "[.]xml$", full.names = TRUE)
  slide_text <- unlist(lapply(slides, readLines, warn = FALSE))
  expect_true(any(grepl("Medical History", slide_text, fixed = TRUE)))
  expect_true(any(grepl("356 (89.0%)", slide_text, fixed = TRUE)))
  expect_true(any(grepl(">1919</a:t>", slide_text, fixed = TRUE)))
  expect_true(any(grepl("Patients are counted once", slide_text, fixed = TRUE)))
})

test_that("character arms retain event-free groups and input records are not modified", {
  dat <- mh_fixture()
  dat$adsl$TRT01A <- as.character(dat$adsl$TRT01A)
  dat$admh$USUBJID <- factor(dat$admh$USUBJID)
  before <- dat
  result <- t_mh_slide(dat$adsl, dat$admh)
  expect_equal(rtables::col_counts(result), c(3, 1, 2, 6))
  expect_identical(dat, before)
  dat$admh$TRT01A <- dat$adsl$TRT01A[match(dat$admh$USUBJID, dat$adsl$USUBJID)]
  dat$admh$TRT01A[1L] <- NA_character_
  expect_identical(mh_matrix(t_mh_slide(dat$adsl, dat$admh)), mh_matrix(result))
})

test_that("SOC sorting uses unique subjects rather than record counts", {
  dat <- mh_fixture()
  # Nervous has many more records, but still fewer affected subjects than Cardiac.
  dat$admh <- rbind(dat$admh, dat$admh[rep(5L, 20L), ])
  labels <- trimws(mh_matrix(t_mh_slide(dat$adsl, dat$admh))[, 1L])
  expect_lt(match("Cardiac disorders", labels), match("Nervous system disorders", labels))
  shuffled <- dat$admh[rev(seq_len(nrow(dat$admh))), ]
  expect_identical(mh_matrix(t_mh_slide(dat$adsl, dat$admh)), mh_matrix(t_mh_slide(dat$adsl, shuffled)))
})

test_that("bundled data has compatible subject, study and treatment identifiers", {
  subject <- match(eg_admh$USUBJID, eg_adsl$USUBJID)
  expect_false(anyNA(subject))
  expect_identical(as.character(eg_admh$STUDYID), as.character(eg_adsl$STUDYID[subject]))
  expect_identical(as.character(eg_admh$TRT01A), as.character(eg_adsl$TRT01A[subject]))
  expect_equal(length(unique(eg_admh$USUBJID)), 356L)
  expect_equal(nrow(eg_admh), 1919L)
})

test_that("FAS filtering propagates from ADSL to MH counts and denominators", {
  testthat::skip_if_not_installed("filters")
  filters::load_filters(system.file("filters.yml", package = "autoslider.core"), overwrite = TRUE)
  dat <- mh_fixture()
  dat$adsl$FASFL <- c("Y", "N", "Y", "Y", "Y", "Y")
  dat$adsl$STUDYID <- "TEST"
  dat$admh$STUDYID <- "TEST"
  spec <- filter_spec(read_spec(system.file("spec.yml", package = "autoslider.core")), program == "t_mh_slide")
  outputs <- generate_outputs(spec, datasets = dat, verbose_level = 0L)
  expected <- t_mh_slide(dat$adsl[dat$adsl$FASFL == "Y", ], dat$admh)
  expect_s4_class(outputs[[1L]], "VTableTree")
  expect_equal(rtables::col_counts(outputs[[1L]]), c(2, 2, 1, 5))
  expect_identical(mh_matrix(outputs[[1L]]), mh_matrix(expected))
})

test_that("copied and renamed MH template executes without hidden imports", {
  dat <- mh_fixture()
  path <- tempfile("mh-template-")
  dir.create(path)

  expect_true(use_template(
    template = "t_mh_slide",
    function_name = "custom_mh_table",
    save_path = path,
    open = FALSE
  ))
  env <- new.env(parent = baseenv())
  sys.source(file.path(path, "custom_mh_table.R"), envir = env)

  copied <- env$custom_mh_table(dat$adsl, dat$admh)
  original <- t_mh_slide(dat$adsl, dat$admh)
  expect_identical(mh_matrix(copied), mh_matrix(original))
})
