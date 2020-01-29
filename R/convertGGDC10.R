#' Convert GGDC10
#' 
#' Convert GGDC10 converts data from readGGDC10() to ISO country level.
#' 
#' @param x MAgPIE object containing GGDC10 data region resolution
#' @return MAgPIE object of the GGDC10 data disaggregated to country level
#' @author Marcos Marcolino, Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertGGDC10(x)
#' }

convertGGDC10 <- function(x){
  
  y <- toolCountryFill(x,fill=0)
  
  return(y)
}
