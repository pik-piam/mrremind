#' Converts EU Effort Sharing targets and historical emissions 
#' 
#' @param x MAgPIE object to be converted
#' @param subtype data subtype. Either "target" or "emissions"
#' @return A MAgPIE object containing the EU Effort Sharing targets (%) or Effort Sharing historical emissions (MtCO2) 
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ a <- convertEurostat_EffortSharing(x,subtype="target")
#' }
#'  

convertEurostat_EffortSharing <- function(x,subtype) {
  # fill countries with no data
  x  <- toolCountryFill(x,fill=0,verbosity=0)
  # replace NAs with 0
  x[is.na(x)] <- 0
 return(x)
 }
 
 