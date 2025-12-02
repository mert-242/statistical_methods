test_that("poisson PMF and CDF work", {
  lambda <- 3
  expect_equal(sum(poisson_pmf(0:15, lambda)), 1, tolerance = 1e-6)
  expect_true(all(diff(poisson_cdf(0:10, lambda)) >= 0))
})
test_that("poisson_moments returns correct mean and variance", {
  lambda <- 3
  m <- poisson_moments(lambda)
  expect_equal(m$mean, lambda)
  expect_equal(m$variance, lambda)
})