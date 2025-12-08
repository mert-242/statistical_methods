#' Moments of the Student's t-Distribution
#'
#' @description Compute the mean and variance for the Student's t-distribution.
#'
#' @param df Degrees of freedom (n).
#'
#' @return A list with elements 'mean' and 'variance'.
#' @export
student_t_moments <- function(df) {
  if (length(df) != 1 || df <= 0) {
    stop("'df' must be positive.")
  }
  
  # Mean = 0 (Chapter 5.2, page 41)
  # Not: Mathematically it should be df > 1, otherwise it is undefined.
  if (df <= 1) {
    mean_val <- NaN 
    warning("Mean is undefined for df <= 1")
  } else {
    mean_val <- 0
  }
  
  # Variance = n / (n - 2) (Chapter 5.2, page 41)
  # Not: Mathematically it should be df > 2.
  if (df <= 2) {
    var_val <- Inf # or NaN
    warning("Variance is undefined or infinite for df <= 2")
  } else {
    var_val <- df / (df - 2)
  }
  
  list(mean = mean_val, variance = var_val)
}