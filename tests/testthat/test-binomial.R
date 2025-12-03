test_that("binomial PMF and CDF work correctly (Book Example p.19)", {
  # Parameters: 10 coin toss, %50 probability of heads
  n <- 10
  p <- 0.5
  
  # Test 1: P(X=4) check if the probability is 0.2051
  # tolerance=1e-4, to tolerate the rounding differences
  expect_equal(binomial_pmf(4, n, p), 0.2051, tolerance = 1e-4)
  
  # Test 2: P(X<=2) check if the probability is 0.0547 
  expect_equal(binomial_cdf(2, n, p), 0.0547, tolerance = 1e-4)
  
  # Extra Test (sum of all probabilities): the sum of all probabilities should be 1
  expect_equal(sum(binomial_pmf(0:n, n, p)), 1)
})

test_that("binomial PMF works for biased coin (Book Example p.20)", {
  # Parameters: biased coin, %70 chance of getting tails
  n <- 10
  p <- 0.7
  
  # example on page 20: P(X=2) = 0.0014
  expect_equal(binomial_pmf(2, n, p), 0.0014, tolerance = 1e-4)
  
  # example on page 20: P(X<=4) = 0.0473
  expect_equal(binomial_cdf(4, n, p), 0.0473, tolerance = 1e-4)
})