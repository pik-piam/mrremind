#' @title toolHoldConstantBeyondEnd
#' @description Holds a historical dataset constant for the entire simulation period "time".
#'
#' @param x MAgPIE object to be continued.
#' @return MAgPIE object with completed time dimensionality.
#' @author Benjamin Leon Bodirsky
#' @importFrom magpiesets findset
#' @importFrom mstools toolHoldConstant
#' @export

toolHoldConstantBeyondEnd<-function(x){
  return(toolHoldConstant(x,years=findset("time")))
}
