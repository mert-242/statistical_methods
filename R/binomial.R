#' Binomial Distribution B(n,p)
#'
#' @family discrete-distributions
#' @description PMF, CDF, and RNG for a binomial distribution with parameters n and p.
#' @details The Binomial distribution models the number of successes in n independent Bernoulli trials
#' with probability of success p. Support is \{0, 1, ..., n\}.
#'
#' @param k Integer value(s) for PMF (x values). looking for k
#' @param q Numeric value(s) for CDF. x <= k
#' @param n Integer number of trials (must be >= 0). how many times i do the experiment
#' @param p Probability of success (must be between 0 and 1). probability
#' @param num_samples Number of random observations to generate.
#'
#' @return
#' \code{binomial_pmf} gives the probability mass.
#' \code{binomial_cdf} gives the cumulative distribution function.
#' \code{binomial_rnd} generates random deviates.
#'
#' @export
binomial_pmf <- function(k, n, p) { #use when a specific number of events are asked, like a coin flip coming as tails for 2 times.
  # error control
  if (length(n) != 1 || n < 0 || n %% 1 != 0) {
    stop("'n' (trials) must be a non-negative integer.")
  }
  if (length(p) != 1 || p < 0 || p > 1) {
    stop("'p' (probability) must be between 0 and 1.")
  }
  
  # make k, a number
  k <- as.numeric(k)
  
  # PMF: f(x) = choose(n, x) * p^x * (1-p)^(n-x)
  #only calculate for the whole numbers between 0 and n. others are 0.
  out <- numeric(length(k))
  valid_indices <- (k >= 0) & (k <= n) & (k %% 1 == 0)
  
  if (any(valid_indices)) {
    # choose(): combination(n, k)
    x_valid <- k[valid_indices]
    out[valid_indices] <- choose(n, x_valid) * (p ^ x_valid) * ((1 - p) ^ (n - x_valid))
  }
  
  return(out)
}

#' @rdname binomial_pmf
#' @export
binomial_cdf <- function(q, n, p) { #use when asked the outcome is lesser than a number, like 3 or fewer heads
  if (length(n) != 1 || n < 0 || n %% 1 != 0) stop("'n' must be a non-negative integer.")
  if (length(p) != 1 || p < 0 || p > 1) stop("'p' must be between 0 and 1.")
  
  q <- as.numeric(q)

  # F(x) = P(X <= x)
  #pbinom func. of R makes it optimal
  return(stats::pbinom(q, size = n, prob = p))
}

#' @rdname binomial_pmf
#' @export
binomial_rnd <- function(num_samples, n, p) { #makes up random k numbers and makes calculations for them
  if (length(n) != 1 || n < 0 || n %% 1 != 0) stop("'n' must be a non-negative integer.")
  if (length(p) != 1 || p < 0 || p > 1) stop("'p' must be between 0 and 1.")
  if (num_samples < 0 || num_samples %% 1 != 0) stop("'num_samples' must be a non-negative integer.")
  
  #rbinom for random number generation:
  return(stats::rbinom(num_samples, size = n, prob = p))
}


binomial_pmf(2, 10, 0.7)
binomial_cdf(4, 10, 0.7)
binomial_pmf(3, 10, 0.17)

