test_that("Convert a complete -- DTC into a date time object", {
  expect_equal(
    convert_dtc_to_dtm("2019-07-18T15:25:52"),
    as_iso_dtm("2019-07-18T15:25:52")
  )
})
