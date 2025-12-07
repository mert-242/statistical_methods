#' Continuous Uniform Distribution U(a,b)
#'
#' @family continuous-distributions
#' @description PDF, CDF, and RNG for a continuous uniform distribution on the interval \code{[a, b]}.
#' @details The continuous uniform distribution describes a variable where all values 
#' within the interval \code{[a, b]} are equally probable.
#' Support is the interval \code{[a, b]}.
#'
#' @param x Numeric value(s) for PDF (density).
#' @param q Numeric value(s) for CDF.
#' @param min Lower limit of the distribution (a).
#' @param max Upper limit of the distribution (b).
#' @param n Number of random observations to generate.
#'
#' @return
#' \code{continuous_uniform_pdf} gives the density.
#' \code{continuous_uniform_cdf} gives the distribution function.
#' \code{continuous_uniform_rnd} generates random deviates.
#'
#' @export
continuous_uniform_pdf <- function(x, min = 0, max = 1) {
  if (length(min) != 1 || length(max) != 1 || min >= max) {
    stop("'min' (a) must be less than 'max' (b).")
  }
  
  # We use R's dunif function
  # Logic: If min<x<max returns 1/(max-min)
  stats::dunif(x, min = min, max = max)
}

#' @rdname continuous_uniform_pdf
#' @export
continuous_uniform_cdf <- function(q, min = 0, max = 1) {
  if (length(min) != 1 || length(max) != 1 || min >= max) {
    stop("'min' (a) must be less than 'max' (b).")
  }
  
  # R's punif function
  # Logic: (q - min) / (max - min)
  stats::punif(q, min = min, max = max)
}

#' @rdname continuous_uniform_pdf
#' @export
continuous_uniform_rnd <- function(n, min = 0, max = 1) {
  if (length(n) != 1 || n < 0 || n %% 1 != 0) stop("'n' must be non-negative.");
  if (length(min) != 1 || length(max) != 1 || min >= max) {
    stop("'min' (a) must be less than 'max' (b).")
  }
  
  # R's runif function
  stats::runif(n, min = min, max = max)
}