#' Student's t-Distribution T(df)
#'
#' @family continuous-distributions
#' @description PDF, CDF, and RNG for the Student's t-distribution with df degrees of freedom.
#' @details The Student's t-distribution is symmetric and bell-shaped, like the normal distribution, 
#' but has heavier tails. It is used extensively in hypothesis testing and confidence intervals 
#' when the population variance is unknown.
#' Support is the set of all real numbers \code{(-Inf, Inf)}.
#'
#' @param x Numeric value(s) for PDF.
#' @param q Numeric value(s) for CDF. q = t-value (sample mean - population mean) / (sample sd / sqrt(n))
#' @param df Degrees of freedom (must be > 0). df = n-1
#' @param n Number of random observations to generate.
#'
#' @return
#' \code{student_t_pdf} gives the density.
#' \code{student_t_cdf} gives the distribution function.
#' \code{student_t_rnd} generates random deviates.
#'
#' @export
student_t_pdf <- function(x, df) {
  if (length(df) != 1 || df <= 0) {
    stop("'df' (degrees of freedom) must be positive.")
  }
  
  # R's dt function
  stats::dt(x, df = df)
}

#' @rdname student_t_pdf
#' @export
student_t_cdf <- function(q, df) {
  if (length(df) != 1 || df <= 0) {
    stop("'df' (degrees of freedom) must be positive.")
  }
  
  # R's pt function
  stats::pt(q, df = df)
}

#' @rdname student_t_pdf
#' @export
student_t_rnd <- function(n, df) {
  if (length(n) != 1 || n < 0 || n %% 1 != 0) stop("'n' must be non-negative.");
  if (length(df) != 1 || df <= 0) {
    stop("'df' (degrees of freedom) must be positive.")
  }
  
  # R's rt function
  stats::rt(n, df = df)
}