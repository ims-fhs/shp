test_that("import_SPSS_file", {
  expect_error(import_SPSS_file())
  # expect_equal(class(import_SPSS_file(
  #   file = "SHP99_P_USER.sav",
  #   path = "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS/W1_1999")),
  #   "data.frame")
})

test_that("import_id", {
  expect_error(import_id())
})

test_that("import_cols", {
  expect_error(import_cols())
  expect_error(import_cols("some_var", data.frame(some_other_name = 1, even_an_other_one = 2)) )
})

test_that("yearly_col_names", {
  expect_error(yearly_col_names())
  expect_error(yearly_col_names("IDPERS", 1998))
  expect_error(yearly_col_names("IDPERS", 2021))
  expect_error(yearly_col_names("nonID"))
  expect_equal(yearly_col_names(c("IDPERS", "PYYF50"), 2008), c("IDPERS", "P08F50"))
  expect_equal(yearly_col_names(c("IDPERS", "PYYF123"), 2008), c("IDPERS", "P08F123"))
  expect_equal(yearly_col_names(c("IDPERS", "AYYA00", "BYYB00"), 2008), c("IDPERS", "A08A00", "B08B00"))
})
