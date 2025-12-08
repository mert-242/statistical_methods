#' Moments of the Chi-Square Distribution
#'
#' @description Compute the mean and variance for the Chi-Square distribution.
#'
#' @param df Degrees of freedom (n).
#'
#' @return A list with elements 'mean' and 'variance'.
#' @export
chi_square_moments <- function(df) {
  if (length(df) != 1 || df <= 0) {
    stop("'df' must be positive.")
  }
  
  # Formulas (Chapter 5.2, page 38)
  # Mean = n
  mean_val <- df
  
  # Variance = 2n
  var_val <- 2 * df
  
  list(mean = mean_val, variance = var_val)
}