library(growthcurver)
context("summarize growth")

test_that("SummarizeGrowth returns expected values", {
  # test that what is returned from the actual equation equals the
  # simulated data
  k_in <- 0.5
  n0_in <- 1e-5
  r_in <- 1.2
  N <- 1000
  data_t <- 0:N * 24 / N
  data_n <- growthcurver:::NAtT(k = k_in, n0 = n0_in, r = r_in, t = data_t)
  gc <- growthcurver::SummarizeGrowth(data_t, data_n, bg_correct = "none")
  expect_equal(k_in, gc$vals$k)
  expect_equal(n0_in, gc$vals$n0)
  expect_equal(r_in, gc$vals$r)
  expect_equal(round(9.02, 2), round(gc$val$t_mid, 2))
  expect_equal(7.49, round(gc$val$auc_l, 2))
  expect_equal(class(gc), "gcfit")
  expect_equal(class(gc$model), "nls")
  expect_equal(gc$data$t[1], 0)
})

