#' Moments of the Geometric Distribution Geom(p)
#'
#' @description Compute the mean and variance for the Geometric distribution.
#' defined as the number of trials to achieve the first success.
#'
#' @param p Probability of success (0 < p <= 1).
#'
#' @return A list with elements 'mean' and 'variance'.
#' @export
geometric_moments <- function(p) {
  if (length(p) != 1 || p <= 0 || p > 1) {
    stop("'p' must be in (0, 1].")
  }
  
  # Formulas (Chapter 5.1, page 21)
  # Mean = 1 / p
  mean_val <- 1 / p
  
  # Variance = (1 - p) / p^2
  var_val <- (1 - p) / (p ^ 2)
  
  list(mean = mean_val, variance = var_val)
}