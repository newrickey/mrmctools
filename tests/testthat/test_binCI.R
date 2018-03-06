library(mrmctools)
context("Check CI formulas")

test_that("CI is returned as x100 with 1 decimal place  (default rounding)", {
 expect_equal(binCI(.23,.1,.4),  "23.0 (10.0 to 40.0)")
})

test_that("CI is returned as x100 with 0 decimal places", {
  expect_equal(binCI(.23,.1,.4,0,T),  "23 (10 to 40)") 
})

test_that("CI is returned as entered with 2 decimal places", {
  expect_equal(binCI(.23,.1,.4,2,F),  "0.23 (0.10 to 0.40)") 
})