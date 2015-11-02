library(growthcurver)
context("Empirical area under curve")

test_that("Empirical area under curve returns right area", {
  # test that what is returned from the actual equation equals the
  # simulated data
  k_in <- 0.5
  n0_in <- 1e-5
  r_in <- 1.2
  N <- 1000
  data_t <- 1:N * 10 / N
  data_n <- NAtT(k = k_in, n0 = n0_in, r = r_in, t = data_t)
  auc_expected <- AreaUnderCurve(k = k_in, n0 = n0_in,
                                 r = r_in, t_min= 0, t_max = 10)
  auc_actual <- EmpiricalAreaUnderCurve(data_t = data_t,
                                           data_n = data_n,
                                           t_trim = max(data_t))
  expect_equal(round(auc_expected$value, 5), round(auc_actual, 5))
})

