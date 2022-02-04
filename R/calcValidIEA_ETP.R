#' Generate IEA ETP data used for validation in historical.mif
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @param varSet either "all" or "only_regi_meaningful" to filter variables that are too imprecise on regional level
#' @export

calcValidIEA_ETP <- function(varSet) {
  
  if (!varSet %in% c("all", "only_regi_meaningful")) {
    stop("Not a valid subtype! Must be either \"all\" or \"only_regi_meaningful\"")
  }
  
  data <- calcOutput("IEA_ETP", aggregate = F)

  if (varSet == "only_regi_meaningful") {
    data <- data[, , c(
      "Production|Industry|Cement (Mt/yr)",
      "Production|Industry|Steel (Mt/yr"
    ), pmatch = T, invert = T]
  }
  
  data <- add_dimension(data, dim = 3.1, add = "scenario", nm = "historical")

  return(list(
    x = data,
    weight = NULL,
    unit = c("EJ/yr", "Mt CO2/yr", "Mt/yr", "bn pkm/yr", "bn tkm/yr"),
    description = "IEA ETP projections as REMIND variables"
  ))
}
