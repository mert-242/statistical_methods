#' Moments of the Normal Distribution N(mu, sigma)
#'
#' @description Compute the mean and variance for the normal distribution.
#'
#' @param mean The mean (mu).
#' @param sd The standard deviation (sigma > 0).
#'
#' @return A list with elements 'mean' and 'variance'.
#' @export
normal_moments <- function(mean = 0, sd = 1) {
  if (length(sd) != 1 || sd <= 0) {
    stop("'sd' (sigma) must be positive.")
  }
  
  # Formulas (Chapter 5.2, page 22)
  # Mean = mu
  mean_val <- mean
  
  # Variance = sigma^2
  var_val <- sd^2
  
  list(mean = mean_val, variance = var_val)
}