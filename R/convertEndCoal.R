#' Data on Coal Plants (MW) from EndCoal.org , July 2018
#' @author Aman Malik
#' @return Magpie object with country level data on coal plants (MW) in various stages
#' @param x MAgPIE object to be converted

convertEndCoal <- function(x){
  # x <- readSource("EndCoal",convert = F)
  # renaming countries that toolCountry2isocode can't identify
  getRegions(x)[getRegions(x) %in% c("The Niger","Ivory Coast",
                                     "Taiwan, China","FYROM")] <- 
  c("Taiwan","Cote d Ivoire","Macedonia","Niger")  
   
  getRegions(x) <- toolCountry2isocode(getRegions(x))
  x <- toolCountryFill(x,fill = 0)
  return(x)
}
  