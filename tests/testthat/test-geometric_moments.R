test_that("geometric_moments calculates standard values correctly", {
  # check with an easy p value: p = 0.5
  p <- 0.5
  m <- geometric_moments(p)
  
  # Mean: 1/p = 1/0.5 = 2
  expect_equal(m$mean, 2)
  
  # Variance: (1-p)/p^2 = 0.5 / 0.25 = 2
  expect_equal(m$variance, 2)
})

test_that("geometric_moments matches textbook example (Lightbulbs)", {
  # Chapter 5.1, page 22
  # p = 3/75 = 0.04
  p <- 0.04
  m <- geometric_moments(p)
  
  # Mean: 1 / 0.04 = 25
  expect_equal(m$mean, 25)
  
  # Variance: (1 - 0.04) / (0.04^2) = 0.96 / 0.0016 = 600
  expect_equal(m$variance, 600)
})

test_that("geometric_moments handles boundary case p=1", {
  # definite event (p=1)
  p <- 1
  m <- geometric_moments(p)
  
  # mean is 1 since it will happen in the first try
  expect_equal(m$mean, 1)
  
  # it is definite so variance=0
  expect_equal(m$variance, 0)
})