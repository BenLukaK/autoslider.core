# Rebuild the bundled synthetic ADMH example data.
#
# Provenance: random.cdisc.data 0.3.16, public `radmh()` generator.
# The fixed seed makes the result reproducible. The generator receives the
# package's existing ADSL so STUDYID, USUBJID, and treatment remain aligned.

generator_version <- "0.3.16"
seed <- 130

if (!requireNamespace("random.cdisc.data", quietly = TRUE)) {
  stop("Install random.cdisc.data version ", generator_version, " to rebuild eg_admh.")
}
if (as.character(utils::packageVersion("random.cdisc.data")) != generator_version) {
  stop("Rebuild eg_admh with random.cdisc.data version ", generator_version, ".")
}

load(file.path("data", "eg_adsl.rda"))
eg_admh <- random.cdisc.data::radmh(eg_adsl, seed = seed)

stopifnot(
  nrow(eg_adsl) == 400L,
  all(eg_admh$USUBJID %in% eg_adsl$USUBJID),
  identical(
    as.character(eg_admh$STUDYID),
    as.character(eg_adsl$STUDYID[match(eg_admh$USUBJID, eg_adsl$USUBJID)])
  ),
  identical(
    as.character(eg_admh$TRT01A),
    as.character(eg_adsl$TRT01A[match(eg_admh$USUBJID, eg_adsl$USUBJID)])
  )
)

save(eg_admh, file = file.path("data", "eg_admh.rda"), compress = "xz")
