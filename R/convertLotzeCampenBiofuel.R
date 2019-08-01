#' convertLotzeCampenBiofuel
#' 
#' Converts the Data from Lotze Campen et al. 2014. to fit the common country list.
#' Source: Lotze Campen et al. 2014. "Impacts of increased bioenergy demand on global food
#' markets: an AgMIP economic model intercomparison" Agricultural Economics 45 (103-116).
#' doi:10.1111/agec.12092.
#' 
#' 
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing future trends in first generation bioenergy demand in
#' Petajoules as magpie object for each country for biodiesel and ethanol.
#' @author Ewerton Araujo
#' @examples
#' 
#' 
#'   \dontrun{
#'     x <- readSource("LotzeCampenBiofuel")
#'   }
#' 
#' 
      
convertLotzeCampenBiofuel <- function(x) {
  x[is.na(x)] <- 0
  countries <- colnames(getAggregationMatrix("country_mapping.csv"))
  pop       <- calcOutput("Population", aggregate=FALSE)  
  x      <- toolAggregate(x=x, rel="country_mapping.csv",weight=pop[countries,2010,1])
  y      <- toolCountryFill(x, fill=0, verbosity=2)
  y=y*1000
  return(y)
}