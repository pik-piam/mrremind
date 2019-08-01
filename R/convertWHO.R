#' @title convertWHO
#' @description Converts data from the WHO
#' @param x unconverted magpie object from read-script
#' 
#' @return magpie object with a completed dataset.
#' 
#' @seealso
#' \code{\link{convertWHO}}


convertWHO <- function(x) 
{
  meanactivity=dimSums(x,dim=1)/length(getRegions(x))
  x<-toolCountryFill(x,fill = NA)
  countries<-where(is.na(x))$true$region
  x[countries,,]<-meanactivity
  vcat(1, "better replace using function")
  out<-x

  return(out)
}
