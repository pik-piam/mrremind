#' @importFrom luscale getAggregationMatrix 

convertIIASABioenergy <- function(x) {
  x[is.na(x)] <- 0
  countries <- colnames(getAggregationMatrix("country_mapping.csv"))
  pop       <- calcOutput("Population",aggregate=FALSE)
  x      <- toolAggregate(x=x, rel="crop_mapping.csv",dim=3)
  x      <- toolAggregate(x=x, rel="country_mapping.csv",weight=pop[countries,2010,1])
  x      <- toolCountryFill(x, fill=0)
  getSets(x) <- c("country","year","crop")
  return(x)
}