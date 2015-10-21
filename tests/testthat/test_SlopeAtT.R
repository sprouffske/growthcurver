context("Find the slope at time t (SlopeAtT)")

test_that("slope is correct at time t", {
  k_in <- 1
  n0_in <- 1e-3
  r_in <- 1.3
 
  expect_equal(0.311919,  
               round(growthcurver:::SlopeAtT(k = k_in, n0 = n0_in, 
                                         r = r_in, t = 5), 6))
})

