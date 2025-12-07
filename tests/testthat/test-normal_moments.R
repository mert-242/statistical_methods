test_that("normal_moments returns correct variance", {
  # If standard deviation is 3, variance should be 9
  m <- normal_moments(mean = 10, sd = 3)
  expect_equal(m$variance, 9)
})