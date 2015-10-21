library(growthcurver)
context("Find the area under the logistic curve (AreaUnderCurve)")

test_that("the correct area under the curve is returned by AreaUnderCurve", {
  k_in <- 0.5
  n0_in <- 1e-5
  r_in <- 1.2
  auc <- growthcurver:::AreaUnderCurve(k = k_in, n0 = n0_in,
                                         r = r_in, t_min = 0, t_max = 10)
  expect_equal(round(0.60338, 4), round(auc$value, 4))
})

