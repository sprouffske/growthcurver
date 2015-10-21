library(growthcurver)
context("Find n at time t (NAtT)")

test_that("NAtT returns the correct n at time t", {
  k_in <- 0.5
  n0_in <- 1e-5
  r_in <- 1.2
 
  expect_equal(n0_in,  
               growthcurver:::NAtT(k = k_in, n0 = n0_in, r = r_in, t = 0))
  expect_equal(k_in,  
               growthcurver:::NAtT(k = k_in, n0 = n0_in, r = r_in, t = 1000))
  expect_equal(round(0.2475303, 5),  
               round(growthcurver:::NAtT(k = k_in, n0 = n0_in, 
                                         r = r_in, t = 9), 5))
})

