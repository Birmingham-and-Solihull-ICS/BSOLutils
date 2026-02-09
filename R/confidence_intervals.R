#
#' Wilson score binomial confidence interval for proportions
#'
#' @description A Wilson score-based CI calculation for a proportion.  Based on guidance by PHE
#'
#'
#' @param o observed or numerator value
#' @param n expected or denominator value
#' @param ci confidence interval coverage required.  Default is 0.95 for 95\% confidence interval
#'
#' @return A vector o/n, lower confidence interval limit, and upper confidence interval limit
#' @export
#'
#' @importFrom stats qnorm
#'
#' @examples
#' prop_ci(50,120)
prop_ci <- function(o, n, ci=0.95){

  z <- qnorm(ci + ((1-ci)/2))
  p <- o/n
  q <- 1-p

  plower <- ((2*o + z^2) - z * sqrt(z^2 + (4*o*q))) / (2*(n+z^2))

  pupper <- ((2*o + z^2) + z * sqrt(z^2 + (4*o*q))) / (2*(n+z^2))

  return(data.frame(Rate=o/n, LowerCI=plower, UpperCI=pupper))

}


#' Byar's confidence interval
#'
#' @description Byar's confidence interval fro counts, crude rates or indirectly standardised ratios
#'
#'
#' @param o observed or numerator value
#' @param n expected or denominator value
#' @param ci confidence interval coverage required.  Default is 0.95 for 95\% confidence interval
#'
#' @return A vector o/n, lower confidence interval limit, and upper confidence interval limit
#' @export
#'
#' @importFrom stats qnorm
#'
#' @examples
#' byars_ci(50, 120)
byars_ci <- function(o, n, ci=0.95){

  z <- qnorm(ci + ((1-ci)/2))

  olower <- o * (( 1 - (1/(9*o)) - (z / (3 * sqrt(o))))^3)

  oupper <- (o+1) * (( 1 - (1/(9*(o+1))) + (z / (3 * sqrt((o+1)))))^3)

  return(data.frame(Rate=o/n, LowerCI=olower/n, UpperCI=oupper/n))

}

#

#' Exact Poisson limit for an SMR / small count (Ulm)
#'
#' @description Exact poisson limit used for small counts and SMRs, based on Poisson distribution.
#' This uses Ulm's method, exploiting the link to the chisq distribution.  This approach is used for un adjusted
#' limits in the FunnelPlotR package.
#'
#' @param o observed or numerator value
#' @param n expected or denominator value
#' @param ci confidence interval coverage required.  Default is 0.95 for 95\% confidence interval
#'
#' @return A vector o/n, lower confidence interval limit, and upper confidence interval limit
#' @export
#'
#' @importFrom stats qnorm qchisq
#'
#' @examples
#' # For a rate of 50 / 100
#' exact_SMR_ci(50, 120)
exact_SMR_ci <- function(o, n, ci=0.95){

  z <- qnorm(ci + ((1-ci)/2))

  olower <- (qchisq(ci + ((1-ci)/2), (2*o), lower.tail = FALSE)/2)
  oupper <- (qchisq(1-(ci + ((1-ci)/2)), 2*(o+1), lower.tail = FALSE)/2)

  return(data.frame(Rate=o/n, LowerCI=olower/n, UpperCI=oupper/n))


}




