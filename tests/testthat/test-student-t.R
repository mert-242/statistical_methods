test_that("student_t CDF matches book critical value logic (Chapter 6, p.28)", {
  n <- 25
  df <- n - 1 # 24
  
  # The critical values used in the pdf are (qt(.975, 24)) around 2.0639.
  # We will check the opposite of it with our CDF function.
  # The result of student_t_cdf(2.0639, 24) should be 0.975.
  
  critical_value <- 2.063899
  prob <- student_t_cdf(critical_value, df)
  
  expect_equal(prob, 0.975, tolerance = 1e-5)
})

test_that("student_t moments respect constraints (p.41)", {
  # Variance test (df > 2)
  df <- 10
  m <- student_t_moments(df)
  
  # Mean = 0
  expect_equal(m$mean, 0)
  
  # Variance = n / (n-2) -> 10 / 8 = 1.25
  expect_equal(m$variance, 1.25)
})

test_that("student_t approaches normal distribution for large df (p.43)", {
  # Chapter 5.2 at page 43 it says that when n -> infinity it gets closer to N(0,1).
  df_large <- 1000
  
  # t-distribution (df=1000) and normal distribution (mean=0, sd=1) 
  # should be nearly same at the point 1.96 (~0.975)
  t_prob <- student_t_cdf(1.96, df_large)
  norm_prob <- stats::pnorm(1.96)
  
  expect_equal(t_prob, norm_prob, tolerance = 1e-3)
})