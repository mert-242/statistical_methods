test_that("binomial_moments returns correct mean and variance", {
  n <- 10
  p <- 0.5
  
  m <- binomial_moments(n, p)
  
  # mean control (Mean = n * p)
  expect_equal(m$mean, 5)
  
  # Variance control (Variance = n * p * (1-p))
  expect_equal(m$variance, 2.5)
})