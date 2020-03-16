#' @title convertBodirsky2018
#' @description Reads in regression parameters estimated using mrregression, and reads in some scenario
#' based on Bodirsky et al not yet published
#' @param x magpie object from read function
#' @param subtype bmi_share, demand_regression, intake_regression or bodyheight_regression for the estimated regression paramters of different regressions. scenarios for scenario projections. 
#' @return magpie object 
#' 
#' @seealso
#' \code{\link{readBodirsky2018}}
#' 
convertBodirsky2018 <- function(x,subtype) {
  if (subtype!="scenarios") {stop("No convert script for this subtype!")}
  return(x)
}  


