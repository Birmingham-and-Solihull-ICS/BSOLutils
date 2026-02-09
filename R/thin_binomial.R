
#' Binomial thinning
#'
#' @param x vector of counts (e.g. arrivals per day)
#' @param p vector of probabilities (e.g. arrivals get admitted)
#'
#' @returns vector of thinned counts
#' @export
#'
#' @examples
#' set.seed(2024)
#' x_counts <- c(0, 3, 10, 5, 2)
#' p        <- 0.4
#' thin_binomial(x_counts, p)

thin_binomial <- function(x, p) {
  stopifnot(is.numeric(x), is.numeric(p), length(p) == 1)
  if (any(x < 0) || any(x %% 1 != 0)) {
    stop("`x` must be non-negative integers.")
  }
  if (p < 0 || p > 1) {
    stop("`p` must be between 0 and 1.")
  }
  rbinom(n = length(x), size = x, prob = p)
}
