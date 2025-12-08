#' Moments of the Fisher-Snedecor F-Distribution
#'
#' @description Compute the mean and variance for the F-distribution.
#'
#' @param df1 Degrees of freedom of the numerator (m).
#' @param df2 Degrees of freedom of the denominator (n).
#'
#' @return A list with elements 'mean' and 'variance'.
#' @export
fisher_f_moments <- function(df1, df2) {
  if (df1 <= 0 || df2 <= 0) {
    stop("Degrees of freedom must be positive.")
  }
  
  # Mean = n / (n - 2) -> df2 / (df2 - 2)
  if (df2 <= 2) {
    mean_val <- NaN
    warning("Mean is undefined for df2 <= 2")
  } else {
    mean_val <- df2 / (df2 - 2)
  }
  
  # Variance formula (Chapter 5.2, page 43)
  # 2 * n^2 * (m + n - 2) / (m * (n - 2)^2 * (n - 4))
  if (df2 <= 4) {
    var_val <- NaN
    warning("Variance is undefined for df2 <= 4")
  } else {
    numerator <- 2 * (df2^2) * (df1 + df2 - 2)
    denominator <- df1 * ((df2 - 2)^2) * (df2 - 4)
    var_val <- numerator / denominator
  }
  
  list(mean = mean_val, variance = var_val)
}