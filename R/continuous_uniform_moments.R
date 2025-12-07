#' Moments of the Continuous Uniform Distribution U(a,b)
#'
#' @description Compute the mean and variance for the continuous uniform distribution.
#'
#' @param min Lower limit (a).
#' @param max Upper limit (b).
#'
#' @return A list with elements 'mean' and 'variance'.
#' @export
continuous_uniform_moments <- function(min = 0, max = 1) {
  if (length(min) != 1 || length(max) != 1 || min >= max) {
    stop("'min' must be less than 'max'.")
  }
  
  # Formulas (Chapter 5.2, Page 14)
  mean_val <- (min + max) / 2
  var_val <- (max - min)^2 / 12
  
  list(mean = mean_val, variance = var_val)
}