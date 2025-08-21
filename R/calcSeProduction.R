#' @title calc secondary energy production
#' @description prepare secondary production data (e.g. electricity generation)
#' to use in historical constraints in the model
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Renato Rodrigues
#'

calcSeProduction <- function() {

  #mapping of remind technology names to IRENA categories
  mappingIRENA <- tibble::tribble(
    ~remind,   ~irena,
    "geohdr",  "Geothermal",
    "hydro",   "Hydropower", # contains renewable hydropower and mixed hydro plants, but not pure pumped storage
    "windon",  "Onshore wind energy",
    "windoff", "Offshore wind energy",
    "spv",     "Solar photovoltaic",
    "csp",     "Concentrated solar power",
    "bioigcc", "Bioenergy",
  )

  histGeneration <- readSource(type = "IRENA", subtype = "Generation") # Units are GWh
  histGeneration <- histGeneration[,, mappingIRENA$irena] / 1000  # Units are TWh
  getNames(histGeneration) <- mappingIRENA$remind
  out <- add_dimension(histGeneration, dim = 3.1, add = NULL, nm = "seel")
  out <- magpiesort(out)

  return(list(x = out, weight = NULL, unit = "TWh/year", description = "production data (e.g. electricity generation)"))
}
