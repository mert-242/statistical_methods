#' Fisher-Snedecor F-Distribution F(df1, df2)
#'
#' @family continuous-distributions
#' @description PDF, CDF, and RNG for the F-distribution with df1 and df2 degrees of freedom.
#' @details The F-distribution arises frequently as the null distribution of a test statistic, 
#' most notably in the analysis of variance (ANOVA) and other F-tests.
#' It is defined as the ratio of two independent chi-square variables, each divided by its degrees of freedom.
#' Support is \code{x > 0}.
#'
#' @param x Numeric value(s) for PDF (must be non-negative).
#' @param q Numeric value(s) for CDF.
#' @param df1 Degrees of freedom of the numerator (m).
#' @param df2 Degrees of freedom of the denominator (n).
#' @param n Number of random observations to generate.
#'
#' @return
#' \code{fisher_f_pdf} gives the density.
#' \code{fisher_f_cdf} gives the distribution function.
#' \code{fisher_f_rnd} generates random deviates.
#'
#' @export
fisher_f_pdf <- function(x, df1, df2) {
  if (df1 <= 0 || df2 <= 0) {
    stop("Degrees of freedom must be positive.")
  }
  
  # R's df function (density F)
  stats::df(x, df1 = df1, df2 = df2)
}

#' @rdname fisher_f_pdf
#' @export
fisher_f_cdf <- function(q, df1, df2) {
  if (df1 <= 0 || df2 <= 0) {
    stop("Degrees of freedom must be positive.")
  }
  
  # R's pf function (probability F)
  stats::pf(q, df1 = df1, df2 = df2)
}

#' @rdname fisher_f_pdf
#' @export
fisher_f_rnd <- function(n, df1, df2) {
  if (length(n) != 1 || n < 0 || n %% 1 != 0) stop("'n' must be non-negative.");
  if (df1 <= 0 || df2 <= 0) {
    stop("Degrees of freedom must be positive.")
  }
  
  # R's rf function (random F)
  stats::rf(n, df1 = df1, df2 = df2)
}