library(testthat)
test_that("fars_summarize_years_2013", {

  z <- fars_summarize_years(2013)
  expect_that(ncol(z),equals(2))
  expect_that(nrow(z),equals(12))
  expect_that(z,is_a("tbl_df"))
})


test_that("fars_map_state", {

  throws_error(fars_map_state(0,2013))
  throws_error(fars_map_state(1,2012))

  })
