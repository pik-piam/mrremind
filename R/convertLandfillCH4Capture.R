#' Convert landfill CH4 capture data
#' 
#' @return DemandModel data as MAgPIE object aggregated to country level
#' @param x MAgPIE object containing original values
#' @author David Chen
#' 

convertLandfillCH4Capture <- function(x){
x <- readSource("LandfillCH4Capture", convert=F)
x <- toolCountryFill(x, fill=0)
return(x)

}