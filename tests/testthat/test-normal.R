test_that("normal distribution solves Cholesterol Example (p.23)", {
  mu <- 210
  sigma <- 20
  
  # The probability of being between 190 and 230 (1 sigma space)
  prob <- normal_cdf(230, mu, sigma) - normal_cdf(190, mu, sigma)
  
  # Expected: Around 0.6827
  expect_equal(prob, 0.6826895, tolerance = 1e-4)
})

test_that("normal distribution solves Exam Example (p.29)", {
  mu <- 60
  sigma <- 10
  
  # Question: P(X >= 70)
  # R's pnorm gives P(X <= x). That's why we do 1 - pnorm
  # Or we can use lower.tail = FALSE parameter but lets 
  # do the math manually here:
  prob_ge_70 <- 1 - normal_cdf(70, mu, sigma)
  
  # Answer in the pdf: 0.1587
  expect_equal(prob_ge_70, 0.1587, tolerance = 1e-4)
})
