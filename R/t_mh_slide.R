#' Medical history table
#'
#' Summarize medical history by system organ class (SOC) and preferred term
#' (PT). ADSL defines both the analysis population and the treatment
#' denominators. ADMH records for subjects not present in ADSL are excluded,
#' and treatment is derived from ADSL. If ADMH also contains `arm`, its
#' non-missing values must agree with ADSL for the same subject.
#'
#' Counts for the overall table and each SOC include the number and percentage
#' of unique subjects with at least one condition and, on a separate row, the
#' non-unique number of condition records. PT rows contain unique subject
#' counts and percentages. SOCs and PTs are sorted by decreasing total unique
#' subject count, with labels used to break ties deterministically. Missing or
#' blank SOC and PT values are displayed as `<Missing>`.
#'
#' @param adsl Subject-level analysis dataset. It must contain one row per
#'   non-missing `USUBJID` and the treatment variable named by `arm`.
#' @param admh Medical history analysis dataset. It must contain `USUBJID`,
#'   `MHBODSYS`, and `MHDECOD`.
#' @param arm Name of the treatment variable in `adsl`, character scalar;
#'   `"TRT01A"` by default.
#' @param add_all_patients_col Logical scalar indicating whether an additional
#'   `All Patients` column is displayed.
#'
#' @return An `rtables::VTableTree` object.
#' @md
#' @inherit gen_notes note
#' @export
#'
#' @references
#' The table structure follows the public MHT01 example in the
#' [TLG Catalog](https://insightsengineering.github.io/tlg-catalog/stable/tables/medical-history/mht01.html).
#'
#' @examples
#' library(dplyr)
#' adsl <- eg_adsl %>%
#'   dplyr::mutate(TRT01A = factor(TRT01A))
#' admh <- eg_admh
#'
#' out <- t_mh_slide(adsl, admh)
#' print(out)
#'
#' out_without_overall <- t_mh_slide(
#'   adsl,
#'   admh,
#'   add_all_patients_col = FALSE
#' )
t_mh_slide <- function(adsl, admh, arm = "TRT01A", add_all_patients_col = TRUE) {
  assertthat::assert_that(is.data.frame(adsl), msg = "`adsl` must be a data frame")
  assertthat::assert_that(is.data.frame(admh), msg = "`admh` must be a data frame")
  assertthat::assert_that(
    is.character(arm) && length(arm) == 1L && !is.na(arm) && nzchar(arm),
    msg = "`arm` must be a non-missing character scalar"
  )
  assertthat::assert_that(
    assertthat::is.flag(add_all_patients_col),
    msg = "`add_all_patients_col` must be TRUE or FALSE"
  )

  required_adsl <- c("USUBJID", arm)
  required_admh <- c("USUBJID", "MHBODSYS", "MHDECOD")
  assertthat::assert_that(
    all(required_adsl %in% names(adsl)),
    msg = paste("`adsl` must contain:", paste(required_adsl, collapse = ", "))
  )
  assertthat::assert_that(
    all(required_admh %in% names(admh)),
    msg = paste("`admh` must contain:", paste(required_admh, collapse = ", "))
  )

  adsl_ids <- as.character(adsl$USUBJID)
  assertthat::assert_that(
    !any(is.na(adsl_ids) | trimws(adsl_ids) == ""),
    msg = "`adsl$USUBJID` must not contain missing or blank values"
  )
  assertthat::assert_that(
    !anyDuplicated(adsl_ids),
    msg = "`adsl` must contain at most one row per `USUBJID`"
  )
  assertthat::assert_that(
    !any(is.na(adsl[[arm]]) | trimws(as.character(adsl[[arm]])) == ""),
    msg = paste0("`adsl$", arm, "` must not contain missing or blank values")
  )

  population <- adsl[, c("USUBJID", arm), drop = FALSE]
  population$USUBJID <- as.character(population$USUBJID)
  if (!is.factor(population[[arm]])) {
    population[[arm]] <- factor(population[[arm]])
  }
  in_population <- as.character(admh$USUBJID) %in% population$USUBJID
  analysis_source <- admh[in_population, , drop = FALSE]
  analysis_source$USUBJID <- as.character(analysis_source$USUBJID)

  if (arm %in% names(analysis_source)) {
    expected_arm <- population[[arm]][match(as.character(analysis_source$USUBJID), population$USUBJID)]
    supplied_arm <- analysis_source[[arm]]
    supplied <- !is.na(supplied_arm) & trimws(as.character(supplied_arm)) != ""
    conflicts <- supplied & as.character(supplied_arm) != as.character(expected_arm)
    assertthat::assert_that(
      !any(conflicts),
      msg = paste0("Non-missing `admh$", arm, "` values must agree with `adsl$", arm, "`")
    )
    analysis_source[[arm]] <- NULL
  }

  analysis <- dplyr::left_join(
    analysis_source,
    population,
    by = "USUBJID"
  )

  normalize_term <- function(x) {
    out <- trimws(as.character(x))
    out[is.na(out) | out == ""] <- "<Missing>"
    out
  }
  analysis$MHBODSYS <- factor(
    normalize_term(analysis$MHBODSYS),
    levels = sort(unique(normalize_term(analysis$MHBODSYS)))
  )
  analysis$MHDECOD <- factor(
    normalize_term(analysis$MHDECOD),
    levels = sort(unique(normalize_term(analysis$MHDECOD)))
  )
  analysis <- formatters::var_relabel(
    analysis,
    MHBODSYS = "MedDRA System Organ Class",
    MHDECOD = "MedDRA Preferred Term"
  )

  if (nrow(analysis) == 0L) {
    result <- autoslider.core::null_report()
    result@main_title <- "Medical History"
    return(result)
  }

  layout <- rtables::basic_table(show_colcounts = TRUE)
  layout <- rtables::split_cols_by(layout, arm)
  if (add_all_patients_col) {
    layout <- rtables::add_overall_col(layout, "All Patients")
  }
  layout <- tern::analyze_num_patients(
    layout,
    "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one condition",
      nonunique = "Total number of conditions"
    )
  )
  layout <- rtables::split_rows_by(
    layout,
    var = "MHBODSYS",
    split_fun = rtables::drop_split_levels,
    child_labels = "visible",
    label_pos = "topleft",
    split_label = formatters::obj_label(analysis$MHBODSYS)
  )
  layout <- tern::summarize_num_patients(
    layout,
    "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one condition",
      nonunique = "Total number of conditions"
    )
  )
  layout <- tern::count_occurrences(layout, vars = "MHDECOD", .indent_mods = -1L)
  layout <- tern::append_varlabels(analysis, "MHDECOD", indent = 1L, lyt = layout)

  result <- rtables::build_table(layout, df = analysis, alt_counts_df = population)
  result <- rtables::prune_table(result)
  result <- rtables::sort_at_path(
    result,
    path = c("MHBODSYS"),
    scorefun = rtables::cont_n_allcols,
    decreasing = TRUE
  )
  result <- rtables::sort_at_path(
    result,
    path = c("MHBODSYS", "*", "MHDECOD"),
    scorefun = tern::score_occurrences,
    decreasing = TRUE
  )
  result@main_title <- "Medical History"
  result
}
