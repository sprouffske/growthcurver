library(growthcurver)
context("Fit logistic")

test_that("FitLogistic returns expected K", {
  k_in <- 0.5
  n0_in <- 1e-5
  r_in <- 1.2
  N <- 500
  data_t <- 0:N * 24 / N
  data_n <- growthcurver:::NAtT(k = k_in, n0 = n0_in, r = r_in, t = data_t)
  nls_mod <- growthcurver:::FitLogistic(data_t, data_n)
  expect_equal(round(k_in, 1), round(summary(nls_mod)$coefficients[1], 1))
})

test_that("FitLogistic returns expected N", {
  k_in <- 0.5
  n0_in <- 1e-5
  r_in <- 1.2
  N <- 500
  data_t <- 0:N * 24 / N
  data_n <- growthcurver:::NAtT(k = k_in, n0 = n0_in, r = r_in, t = data_t)
  nls_mod <- growthcurver:::FitLogistic(data_t, data_n)
  expect_equal(round(n0_in, 1), round(summary(nls_mod)$coefficients[2], 1))
})

test_that("FitLogistic returns expected r", {
  k_in <- 0.5
  n0_in <- 1e-5
  r_in <- 1.2
  N <- 500
  data_t <- 0:N * 24 / N
  data_n <- growthcurver:::NAtT(k = k_in, n0 = n0_in, r = r_in, t = data_t)
  nls_mod <- growthcurver:::FitLogistic(data_t, data_n)
  expect_equal(round(r_in, 2), round(summary(nls_mod)$coefficients[3], 2))
})

