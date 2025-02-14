adsl <- tibble::tribble(
  ~STUDYID, ~USUBJID,
  "TEST01", "PAT01",
  "TEST01", "PAT02"
)

ds <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~DSCAT, ~DSDECOD, ~DSSTDTC,
  "TEST01", "PAT01", "PROTOCOL MILESTONE", "INFORMED CONSENT OBTAINED", "2021-04-01",
  "TEST01", "PAT01", "PROTOCOL MILESTONE", "RANDOMIZATION", "2021-04-11",
  "TEST01", "PAT01", "DISPOSITION EVENT", "ADVERSE EVENT", "2021-12-01",
  "TEST01", "PAT01", "OTHER EVENT", "DEATH", "2022-02-01",
  "TEST01", "PAT02", "PROTOCOL MILESTONE", "INFORMED CONSENT OBTAINED", "2021-04-02",
  "TEST01", "PAT02", "PROTOCOL MILESTONE", "RANDOMIZATION", "2021-04-11",
  "TEST01", "PAT02", "DISPOSITION EVENT", "COMPLETED", "2021-12-01",
  "TEST01", "PAT02", "OTHER EVENT", "DEATH", "2022-04"
)

test_that("Derive RFICDT", {
  expected_output <- tibble::tribble(
    ~STUDYID, ~USUBJID, ~RFICDT,
    "TEST01", "PAT01", as.Date("2021-04-01"),
    "TEST01", "PAT02", as.Date("2021-04-02")
  )
  actual_output <- adsl %>%
    derive_disposition_dt(
      dataset_ds = ds,
      new_var = RFICDT,
      dtc = DSSTDTC,
      filter = DSCAT == "PROTOCOL MILESTONE" & DSDECOD == "INFORMED CONSENT OBTAINED",
      date_imputation = NULL
    )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("STUDYID", "USUBJID")
  )
})


test_that("Derive RANDDT from the relevant ds.DSSTDTC", {
  expected_output <- tibble::tribble(
    ~STUDYID, ~USUBJID, ~RANDDT,
    "TEST01", "PAT01", as.Date("2021-04-11"),
    "TEST01", "PAT02", as.Date("2021-04-11")
  )
  actual_output <- adsl %>%
    derive_disposition_dt(
      dataset_ds = ds,
      new_var = RANDDT,
      dtc = DSSTDTC,
      filter = DSCAT == "PROTOCOL MILESTONE" & DSDECOD == "RANDOMIZATION",
      date_imputation = NULL
    )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("STUDYID", "USUBJID")
  )
})

test_that("Derive DTHDT from the relevant ds.DSSTDTC, impute partial death dates with 1st day/month", { # nolint
  expected_output <- tibble::tribble(
    ~STUDYID, ~USUBJID, ~DTHDT,
    "TEST01", "PAT01", as.Date("2022-02-01"),
    "TEST01", "PAT02", as.Date("2022-04-01")
  )
  actual_output <- adsl %>%
    derive_disposition_dt(
      dataset_ds = ds,
      new_var = DTHDT,
      dtc = DSSTDTC,
      filter = DSCAT == "OTHER EVENT" & DSDECOD == "DEATH",
      date_imputation = "FIRST"
    )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("STUDYID", "USUBJID")
  )
})
