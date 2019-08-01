#' Convert RemindCesPrices
#' 
#' Converts CES derivatives/prices from former REMIND runs to ISO level
#' 
#' 
#' @return magpie object of REMIND prices
#' @author Antoine Levesque
#' @seealso \code{\link{readSource}}
#' @param x MAgPIE object containing REMIND prices at the REMIND region resolution
#' @param subtype Regional resolution of REMIND data which should be loaded. ccd632d33a corresponds to the REMIND-11,
#'  and 690d3718e1 to REMIND-H12
#' 
#' 
convertRemindCesPrices <- function(x, subtype = "ccd632d33a") {
  
  #-----FUNCTIONS--------
  #-----END FUNCTIONS----
  
  
  if (!subtype %in% c("ccd632d33a", "690d3718e1"))  stop("valid subtypes are 'ccd632d33a', '690d3718e1'")
  if (subtype == "ccd632d33a") mappingfile <- toolMappingFile("regional","regionmappingREMIND.csv")
  if (subtype == "690d3718e1") mappingfile <- toolMappingFile("regional","regionmappingH12.csv")
  
  
  x = toolAggregate(x,mappingfile, weight = NULL)
  
  return(x)
}  
