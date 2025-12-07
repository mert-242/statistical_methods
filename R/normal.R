#' Normal Distribution N(mu, sigma)
#'
#' @family continuous-distributions
#' @description PDF, CDF, and RNG for a normal distribution with mean mu and standard deviation sigma.
#' @details The normal distribution (Gaussian distribution) is defined by its mean and standard deviation.
#' The standard normal distribution occurs when mu = 0 and sigma = 1.
#' Support is the set of all real numbers \code{(-Inf, Inf)}.
#'
#' @param x Numeric value(s) for PDF (density).
#' @param q Numeric value(s) for CDF.
#' @param mean The mean (mu) of the distribution (default = 0).
#' @param sd The standard deviation (sigma) of the distribution (must be > 0, default = 1).
#' @param n Number of random observations to generate.
#'
#' @return
#' \code{normal_pdf} gives the density.
#' \code{normal_cdf} gives the distribution function.
#' \code{normal_rnd} generates random deviates.
#'
#' @examples
#' normal_pdf(0, mean = 0, sd = 1)
#' normal_cdf(1.96, mean = 0, sd = 1)
#'
#' @export
normal_pdf <- function(x, mean = 0, sd = 1) {
  if (length(sd) != 1 || sd <= 0) {
    stop("'sd' (sigma) must be positive.")
  }
  
  # R's dnorm function
  stats::dnorm(x, mean = mean, sd = sd)
}

#' @rdname normal_pdf
#' @export
normal_cdf <- function(q, mean = 0, sd = 1) {
  if (length(sd) != 1 || sd <= 0) {
    stop("'sd' (sigma) must be positive.")
  }
  
  # R's pnorm function
  stats::pnorm(q, mean = mean, sd = sd)
}

#' @rdname normal_pdf
#' @export
normal_rnd <- function(n, mean = 0, sd = 1) {
  if (length(n) != 1 || n < 0 || n %% 1 != 0) stop("'n' must be non-negative.");
  if (length(sd) != 1 || sd <= 0) {
    stop("'sd' (sigma) must be positive.")
  }
  
  # R's rnorm function
  stats::rnorm(n, mean = mean, sd = sd)
}