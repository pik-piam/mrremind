#' Disaggregates IEA WEO 2021 Data
#' @param x MAgPIE object to be converted
#' @return A [`magpie`][magclass::magclass] object.
#' @author Falk Benke
#' @importFrom madrat getISOlist
#'

convertIEA_WEO_2021 <- function(x) {

  PE <- calcOutput("PE", aggregate = FALSE)

  # for now, we only have complete data on global level
  x.world <- x["World",,]

  # to integrate the data in historical.mif, we need to disaggregate to country level
  # the disaggregation is very unprecise and therefore values below gloabl granularity
  # are not reliable
  
  mapping_world <- tibble(
    regions = "World",
    country = getISOlist()
  )

  weight <- PE[, 2016, "PE (EJ/yr)"]
  x.disaggregated <- toolAggregate(x.world, rel = mapping_world, weight = weight)

  return(x.disaggregated)
}
