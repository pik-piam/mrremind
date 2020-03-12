#' @title convertBodirsky2018
#' @description Converts data from Bodirsky2018
#' @param x magpie object provided by the read function
#' @param subtype bmi_share, demand_regression, intake_regression or bodyheight_regression for the estimated regression paramters of different regressions. scenarios for scenario projections. 
#' @return magpie object 
#' 
#' @examples
#' \dontrun{
#'   readSource("Bodirsky", subtype="bmi_shr", convert=TRUE)
#' }
#' 
#' @author Kristine Karstens
#' @seealso
#' \code{\link{readBodirsky}}
#' 
convertBodirsky2018 <- function(x,subtype="bmi_shr") {
  
  return(x)
}  


