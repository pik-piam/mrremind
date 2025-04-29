#' Calculate historical GHG Emissions from European Environment Agency
#'
#' @author Falk Benke, Renato Rodrigues
#'
calcEEAGHGEmissions <- function() {

  eeaSectoral <- readSource("EEA_EuropeanEnvironmentAgency", subtype = "sectoral")
  eeaTotal <- readSource("EEA_EuropeanEnvironmentAgency", subtype = "total")

  eeaSectoral <- magclass::matchDim(eeaSectoral, eeaTotal, dim = 2)

  x <- mbind(eeaSectoral, eeaTotal)
  x <- toolFillEU34Countries(x)

  return(list(
    x = x,
    weight = NULL,
    unit = "Mt CO2-equiv/yr",
    description = "Historical GHG emissions from European Environment Agency"
  ))
}
