#' @title convertACCMIP
#' @description function to convert ACCMIP data to isocountry resolution
#'
#' @param x MAgPIE object
#'
#' @return MAgPIE object
#' 
#' @author Roman Popov
#'
#' @examples
#' \dontrun{
#' readSource("ACCMIP", subtype = "nhx_1850")
#' }
convertACCMIP <- function(x){
  map <- toolMappingFile(type="cell", readcsv=T, name="CountrytoCellMapping.csv")
  getRegions(x) <- map$cell
  y <- toolAggregate(x, map, from=1, to=3, dim=1)
  y <- toolCountryFill(y, fill=NA)
  return(y)
}