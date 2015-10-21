context("Find doubling time at time t (DtAtT)")

test_that("DtAtT returns the correct doubling time at time t", {
  k_in <- 0.5
  n0_in <- 1e-5
  r_in <- 1.2
 
  expect_equal(round(0.577631, 5),  
               round(growthcurver:::DtAtT(k = k_in, 
                                          n0 = n0_in, 
                                          r = r_in, 
                                          t = 0), 5))
})

