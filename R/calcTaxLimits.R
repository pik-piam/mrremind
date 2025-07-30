#' @title calc Tax Limits
#' @description tax and subsidies maximum levels. The tax limits serve as a work
#' around to avoid excess of subsidy levels that could cause problems on the REMIND
#' model solution. These files should be removed or replaced once a better way
#' to handle this issue is introduced to the REMIND model formulation or once
#' better yearly and country subsidy level data is available for the primary
#' and final energies.
#'
#' @param subtype Name of the subsidy data type limit, e.g. "maxFeSubsidy" for
#' maximum final energy subsidy or "propFeSubsidy" for proportional cap for
#' final energy subsidy
#' @return magpie object of the subtype tax limit
#' @author Renato Rodrigues
#' @examples
#' \dontrun{
#' calcOutput("TaxLimits")
#' }
#'
calcTaxLimits <- function(subtype) {

  if (!(subtype %in% c("maxFeSubsidy"))) {
    stop("the argument subtype must be in c('maxFeSubsidy', 'propFeSubsidy')")
  }

  if (subtype == "maxFeSubsidy") {
    # Read max final energy subsidy levels
    output <- readSource("REMIND_11Regi", subtype = "maxFeSubsidy")[, , c("fegas", "fehos", "fesos")]
    description <- "maximum final energy subsidy levels (in $/Gj) from REMIND version prior to rev. 5429"
    # using final energy to weight the max subsidy levels
    weight <- calcOutput("FE", aggregate = FALSE)[, 2005, "FE (EJ/yr)"]
  }

  # Return tax convergence levels aggregated to selected REMIND regions
  return(list(x = output, weight = weight, unit = "US$2017/GJ", description = description))
}
