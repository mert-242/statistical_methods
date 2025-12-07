test_that("continuous_uniform solves the Bus Waiting Time example (p.17)", {
  # Parameters: The bus comes between every 15 minutes, lower and upper bounds are 0 and 15
  a <- 0
  b <- 15
  
  # Question: The probability of waiting between 5 and 10 minutes
  # Mathematical: P(5 <= X <= 10) = CDF(10) - CDF(5)
  prob <- continuous_uniform_cdf(10, a, b) - continuous_uniform_cdf(5, a, b)
  
  # Expected result: 1/3
  expect_equal(prob, 1/3)
})

