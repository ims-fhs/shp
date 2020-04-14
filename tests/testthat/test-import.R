test_that("import_SPSS_file", {
  expect_error(import_SPSS_file())
  # expect_equal(class(import_SPSS_file(
  #   file = "SHP99_P_USER.sav",
  #   path = "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W1_1999")),
  #   "data.frame")
})

test_that("import_id", {
  expect_error(import_id())
})

test_that("import_cols", {
  expect_error(import_cols())
})
