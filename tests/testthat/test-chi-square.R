test_that("chi_square CDF matches book critical value (Chapter 6, p.76)", {
  df <- 4
  critical_value <- 9.488
  
  # The 9.488 given in the pdf, should be equal to %95 trust level (0.05 significance)
  probability <- chi_square_cdf(critical_value, df)
  
  # Should be really close to 0.95
  expect_equal(probability, 0.95, tolerance = 1e-3)
})
