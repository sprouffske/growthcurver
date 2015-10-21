context("Find maximum doubling time (MaxDt)")

test_that("MaxDt returns the correct doubling time", {
  r_in <- 1.2
 
  expect_equal(round(log(2) / r_in, 5),  
               round(growthcurver:::MaxDt(r = r_in), 5))
})

