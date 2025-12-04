#' Geometric Distribution Geom(p)
#'
#' @family discrete-distributions
#' @description PMF, CDF, and RNG for a Geometric distribution with probability p.
#' @details The Geometric distribution models the number of trials required to get the first success.
#' According to the provided textbook definition, X denotes the position of the first occurrence.
#' Support is the set \code{\\{1, 2, 3, ...\\}}.
#'
#' @param k Integer value(s) for PMF (trial number, k >= 1).
#' @param q Numeric value(s) for CDF.
#' @param p Probability of success (must be between 0 and 1).
#' @param n Number of random observations to generate.
#'
#' @return
#' \code{geometric_pmf} gives the probability mass.
#' \code{geometric_cdf} gives the cumulative distribution function.
#' \code{geometric_rnd} generates random deviates (representing trial numbers).
#'
#' @examples
#' geometric_pmf(6, 0.04)
#'
#' @export
geometric_pmf <- function(k, p) {
  if (length(p) != 1 || p <= 0 || p > 1) {
    stop("'p' must be in (0, 1].")
  }
  
  k <- as.numeric(k)
  
  # k is the number of tries (1, 2, ...).
  # R's dgeom founction uses the failure number (x = k-1) 
  # Formula: p * (1-p)^(k-1)
  
  # Calculate only for the numbers that are k >= 1
  out <- numeric(length(k))
  valid <- (k >= 1) & (k %% 1 == 0)
  
  if (any(valid)) {
    # We make it k-1 when using R functions
    out[valid] <- stats::dgeom(k[valid] - 1, prob = p)
  }
  
  return(out)
}

#' @rdname geometric_pmf
#' @export
geometric_cdf <- function(q, p) {
  if (length(p) != 1 || p <= 0 || p > 1) {
    stop("'p' must be in (0, 1].")
  }
  
  q <- as.numeric(q)
  
  # CDF: P(X <= q)
  # if q < 1 then the probability is 0.
  # We again give R's pgeom function (q-1)
  
  out <- numeric(length(q))
  valid <- q >= 1
  
  if (any(valid)) {
    out[valid] <- stats::pgeom(q[valid] - 1, prob = p)
  }
  
  return(out)
}

#' @rdname geometric_pmf
#' @export
geometric_rnd <- function(n, p) {
  if (length(n) != 1 || n < 0 || n %% 1 != 0) stop("'n' must be non-negative.")
  if (length(p) != 1 || p <= 0 || p > 1) stop("'p' must be in (0, 1].")
  
  # R's rgeom function creates the "number of failures"(0, 1, 2...).
  # We add +1 to the result to find the "number of the success".
  return(stats::rgeom(n, prob = p) + 1)
}


geometric_pmf(6,0.04)
