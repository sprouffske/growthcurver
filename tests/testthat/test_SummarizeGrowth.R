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

test_that("SummarizeGrowth does not crash when given unsolvable data", {
  data_t <- 1:20
  data_n <- c(0.003002320, 0.002993543, 0.003001265, 0.003009611, 0.003003090,
              0.003010240, 0.002993298, 0.003007662, 0.002999340, 0.002993723,
              0.003006262, 0.002993025, 0.002979970, 0.003005952, 0.002993092,
              0.002981055, 0.003000662, 0.003000605, 0.003004850, 0.003011140)
  gc <- growthcurver::SummarizeGrowth(data_t, data_n, bg_correct = "min")
  expect_equal("cannot fit data", gc$vals$note)
  expect_equal(0, gc$vals$k)
  expect_equal(0, gc$vals$n0)
  expect_equal(0, gc$vals$r)
})
