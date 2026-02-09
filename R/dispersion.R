#' Dispersion ratio
#'
#' @description This function calculates the dispersion ratio of fitted models. Aimed at binomial and Poisson family models where
#' The fixed variance assumption commonly leads to over- dispersion in the real world.
#'
#'
#' @param model a fitted regression model object that has a relevant pearson residual to be extracted
#' @param ... dots
#'
#' @return A dispersion ratio, where 1 is equidispersion (as expected), > 1 is over-dispersion and <1 is under-dispersion
#' @export
#'
#' @importFrom stats glm df.residual residuals
#'
#' @examples
#' library(NHSRdatasets)
#' data(LOS_model)
#'
#' mod1 <- glm(Death ~ Age * LOS, data=LOS_model, family="binomial")
#'
#' disp_ratio(mod1)
disp_ratio<-function(model, ...){
  sum(residuals(model, type="pearson")^2) / df.residual(model)
}



#' Calculate overdispersion ratio of z-scores
#'
#' @description Internal function to perform the transformations for data types.
#'
#' @param n Single numeric value for the count of the number of groups (and therefore z-scores)
#' @param zscores Vector of z-scores z-scores to be used.  Commonly, this might be 'winsorised' first to remove impact of extreme outliers.
#'
#' @return A numeric phi value
#' @export
#'
#' @examples
#' phi_func(3, c(1.3,0.75, 1.5))
#'
phi_func <- function(n, zscores){
  phi <- (1 / n) * sum(zscores^2)

  return(phi)
}


#' Calculate the between group standard error (tau2) using a dispersion factor, and within
#'
#' @description Function to calculate between group variance (tau2) to add to within group variance (S2).
#' NOTE: the S input, is the within group standard error (the square root of the variance).
#'
#' @param n The number of groups for data items, e.g. hospitals trusts that z-scores are calculated at.
#' @param phi The dispersion ratio, where > 1 means overdispersion
#' @param S Standard error (within cluster, calculated in z-score process)
#'
#' @export
#'
#' @return A numeric Tau2 (between group variance) value
#'
#'
tau_func <- function(n,  phi, S){

  if(length(S) == 0){
    Tau2 <- 0
  } else {
    if((n*phi) < (n - 1)){
      Tau2 <- 0
    } else {

      Tau2 <- max(0, ((sum(n) * sum(phi)) - (sum(n) - 1)) /
                    (sum(1/(S^2)) - (sum((1/(S^2))^2) / sum(1/(S^2)))))
    }
  }

  return(Tau2)
}
