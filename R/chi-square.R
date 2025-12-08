#' Chi-Square Distribution Chi^2(df)
#'
#' @family continuous-distributions
#' @description PDF, CDF, and RNG for the Chi-Square distribution with df degrees of freedom.
#' @details The Chi-Square distribution is widely used in hypothesis testing, 
#' particularly for tests of goodness of fit and independence in contingency tables.
#' It is defined for \code{x >= 0}.
#'
#' @param x Numeric value(s) for PDF (must be non-negative).
#' @param q Numeric value(s) for CDF.
#' @param df Degrees of freedom (must be > 0).
#' @param n Number of random observations to generate.
#'
#' @return
#' \code{chi_square_pdf} gives the density.
#' \code{chi_square_cdf} gives the distribution function.
#' \code{chi_square_rnd} generates random deviates.
#'
#' @export
chi_square_pdf <- function(x, df) {
  if (length(df) != 1 || df <= 0) {
    stop("'df' (degrees of freedom) must be positive.")
  }
  
  # R's dchisq function
  stats::dchisq(x, df = df)
}

#' @rdname chi_square_pdf
#' @export
chi_square_cdf <- function(q, df) {
  if (length(df) != 1 || df <= 0) {
    stop("'df' (degrees of freedom) must be positive.")
  }
  
  # R's pchisq function
  stats::pchisq(q, df = df)
}

#' @rdname chi_square_pdf
#' @export
chi_square_rnd <- function(n, df) {
  if (length(n) != 1 || n < 0 || n %% 1 != 0) stop("'n' must be non-negative.");
  if (length(df) != 1 || df <= 0) {
    stop("'df' (degrees of freedom) must be positive.")
  }
  
  # R's rchisq function
  stats::rchisq(n, df = df)
}