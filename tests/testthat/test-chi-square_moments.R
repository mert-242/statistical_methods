test_that("chi_square_moments returns correct formulas", {
  df <- 10
  m <- chi_square_moments(df)
  
  # Mean = df = 10
  expect_equal(m$mean, 10)
  
  # Variance = 2 * df = 20
  expect_equal(m$variance, 20)
})