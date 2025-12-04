test_that("geometric PMF matches book example (Lightbulbs, p.22)", {
  # Parameters
  p <- 3/75 # 0.04
  k <- 6
  
  # Expected result: 0.0326
  # tolerance=1e-4 to prevent rounding differences
  expect_equal(geometric_pmf(k, p), 0.0326, tolerance = 1e-4)
})

test_that("geometric moments formulas are correct", {
  p <- 0.5
  
  m <- geometric_moments(p)
  
  # Mean = 1/0.5 = 2
  expect_equal(m$mean, 2)
  
  # Variance = (1-0.5) / 0.5^2 = 0.5 / 0.25 = 2
  expect_equal(m$variance, 2)
})

test_that("geometric CDF covers full probability", {
  p <- 0.5
  # The cumulative result of the first 20 tries must be really close to 1
  prob_sum <- geometric_cdf(20, p)
  expect_equal(prob_sum, 1, tolerance = 1e-4)
})