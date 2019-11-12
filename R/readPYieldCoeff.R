#' Read in coefficients for calculating pasture intensification
#' 
#' @description 
#' Read in csv file containing coefficients of linear regression
#' for the calculation of future pasture intensification dependent 
#' on animal numbers
#'
#' @return MAgPIE object
#' @author Isabelle Weindl
#' @seealso \code{\link{readSource}}
#' @export
#'
#' @examples
#' \dontrun{
#' a <- readSource("PYieldCoeff")
#' }
#' @importFrom magclass read.magpie 
readPYieldCoeff <- function(){
  file <-  "pyieldcoeff.csv"
  x<-read.magpie(file)
  return(x)
}