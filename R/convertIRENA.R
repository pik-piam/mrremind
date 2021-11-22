#' Converts IRENA Regional data
#' 
#' @param x MAgPIE object to be converted
#' @param subtype data subtype. Either "Capacity" or "Generation"
#' @return A MAgPIE object containing IRENA country disaggregated data  with historical electricity renewable capacities (MW) or generation levels (GWh) 
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ a <- convertIRENA(x,subtype="Capacity")
#' }
#'  

convertIRENA <- function(x,subtype) {
  # rename countries to REMIND iso codes
  getRegions(x) <- gsub("\\*", "", getRegions(x))
  getRegions(x) <- gsub("South Georgia", "South Georgia and the South Sandwich Islands", getRegions(x),fixed = T)
  getRegions(x) <- toolCountry2isocode(getRegions(x))
  x[is.na(x)] <- 0
  # aggregate Kosovo to Serbia
  x1 <- x["KOS",,]
  getRegions(x1) <- c("SRB")
  x["SRB",,] <- x["SRB",,] + x1
  x <- x[c("KOS"),,,invert=TRUE]
  # fill countries with no data
  x  <- toolCountryFill(x,fill=0,verbosity=0)
  return(x)
 }
 
 
