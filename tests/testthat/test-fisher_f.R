test_that("fisher_f CDF calculates probability correctly", {
  # A simple F value check
  df1 <- 5
  df2 <- 10
  x <- 1
  
  # Comparison with R's integrated pf function.
  prob <- fisher_f_cdf(x, df1, df2)
  expected <- stats::pf(x, df1, df2)
  
  expect_equal(prob, expected)
})

test_that("fisher_f moments match formulas (p.43)", {
  # It should be df2 > 4  so variance can be calculated
  m <- 10 # df1
  n <- 10 # df2
  
  moments <- fisher_f_moments(m, n)
  
  # Mean = n / (n-2) = 10 / 8 = 1.25
  expect_equal(moments$mean, 1.25)
  
  # Variance Calculation
  # 2 * 100 * (10 + 10 - 2) / (10 * 64 * 6)
  # 200 * 18 / 3840 = 3600 / 3840 = 0.9375
  expect_equal(moments$variance, 0.9375)
})

test_that("fisher_f property inverse holds (p.45)", {
  # Chapter 5.2 page 45: F(m, n) = 1 / F(n, m) property
  # This property holds also for quantile (reverse CDF) functions.
  # F_alpha(m, n) = 1 / F_1-alpha(n, m)
  
  df1 <- 5
  df2 <- 10
  alpha <- 0.05
  
  # Critical value for F(m, n)
  f_val <- stats::qf(alpha, df1, df2)
  
  # Complementary critical value for F(n, m)
  f_val_inv <- stats::qf(1 - alpha, df2, df1)
  
  # Product of them should be around 1 (Teori: f_p(m,n) = 1 / f_1-p(n,m))
  expect_equal(f_val * f_val_inv, 1, tolerance = 1e-5)
})