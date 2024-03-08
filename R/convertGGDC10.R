#' Convert GGDC 10-Sector Database - https://www.rug.nl/ggdc/structuralchange/previous-sector-database/10-sector-2014
#' @author Renato Rodrigues
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing GGDC disaggregated data
#' @examples
#'  \dontrun{
#' a <- convertGGDC10(x)
#' }
#'
#' @importFrom madrat toolCountryFill

convertGGDC10 <- function(x) {

  #removing unnecessary regions
  data <- x[!(getItems(x, dim = 1) %in% c("NGA(alt)", "DEW", "MOR")), , ]
  data[is.na(data)] <- 0
  data <- toolCountryFill(data, fill = 0, verbosity = 2)

  return(data)

}
