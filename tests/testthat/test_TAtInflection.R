library(growthcurver)
context("Find the inflection point of the logistic equation")

test_that("The time t at the inflection point is as expected", {
  k_in <- 0.5
  n0_in <- 1e-5
  r_in <- 1.2
  expect_equal(round(log((k_in - n0_in) / n0_in ) / r_in, 5),
               round(TAtInflection(k_in, n0_in, r_in), 5))

})
