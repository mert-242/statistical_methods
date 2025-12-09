#' Two Sample Statistics Helper Functions
#'
#' @description Functions to calculate Pooled Variance and Welch's Degrees of Freedom
#' based on the formulas provided in Chapter 6 (Page 21/30).
#'
#' @param A Numeric vector for the first group.
#' @param B Numeric vector for the second group.
#'
#' @return Numeric value of the calculated statistic.
#' @export

# 1. Pooled Variance Calculator
# Used when population variances are equal (Equal Variances)
pooled_variance <- function(A,B) {
  n1 <- length(A)
  n2 <- length(B)
  s1_sq <- stats::var(A)
  s2_sq <- stats::var(B)
  
  if (n1 <= 1 || n2 <= 1) stop("Sample sizes must be greater than 1.")
  
  # Formula: ((n1 - 1)s1^2 + (n2 - 1)s2^2) / (n1 + n2 - 2)
  numerator <- ((n1 - 1) * s1_sq) + ((n2 - 1) * s2_sq)
  denominator <- n1 + n2 - 2
  
  return(numerator / denominator)
}

# 2. Welch's Degrees of Freedom (f) Calculator
# Used when population variances are different (Unequal Variances)
# NOT: This formula is the special Welch approach from page 21
welch_df_special <- function(A,B) {
  n1 <- length(A)
  n2 <- length(B)
  s1_sq <- stats::var(A)
  s2_sq <- stats::var(B)
  
  if (n1 <= 1 || n2 <= 1) stop("Sample sizes must be greater than 1.")
  
  # Dividing the terms (for a cleaner code)
  term1 <- s1_sq / n1
  term2 <- s2_sq / n2
  
  # Numerator: (s1^2/n1 + s2^2/n2)^2
  numerator <- (term1 + term2)^2
  
  # Denominator:
  # Part 1: (s1^2/n1)^2 / (n1 + 1)
  denom1 <- (term1^2) / (n1 + 1)
  
  # Part 2: (s2^2/n2)^2 / (n2 + 1)
  denom2 <- (term2^2) / (n2 + 1)
  
  # Full Formula: Numerator / (Denom1 + Denom2) - 2
  f_value <- (numerator / (denom1 + denom2)) - 2
  
  return(f_value)
}