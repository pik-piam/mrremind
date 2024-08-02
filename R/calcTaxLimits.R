#' @title calc Tax Limits
#' @description tax and subsidies maximum levels. The tax limits serve as a work
#' around to avoid excess of subsidy levels that could cause problems on the REMIND
#' model solution. These files should be removed or replaced once a better way
#' to handle this issue is introduced to the REMIND model formulation or once
#' better yearly and country subsidy level data is available for the primary
#' and final energies.
#'
#' @param subtype Name of the subsidy data type limit, e.g. "maxFeSubsidy" for
#' maximum final energy subsidy,"maxPeSubsidy" for maximum primary energy subsidy
#' or "propFeSubsidy" for proportional cap for final energy subsidy
#' @return magpie object of the subtype tax limit
#' @author Renato Rodrigues
#' @examples
#' \dontrun{
#' calcOutput("TaxLimits")
#' }
#'
calcTaxLimits <- function(subtype) {

  if (!(subtype %in% c("maxFeSubsidy", "maxPeSubsidy", "propFeSubsidy"))) {
    stop("the argument subtype must be in c('maxFeSubsidy','maxPeSubsidy', 'propFeSubsidy')")
  }

  if (subtype == "maxFeSubsidy") {
    # Read max final energy subsidy levels
    output <- readSource("REMIND_11Regi", subtype = "maxFeSubsidy")[, , c("fegas", "fehos", "fesos")]
    description <- "maximum final energy subsidy levels (in $/Gj) from REMIND version prior to rev. 5429"
    # using final energy to weight the max subsidy levels
    weight <- calcOutput("FE", aggregate = FALSE)[, 2005, "FE (EJ/yr)"]
  } else if (subtype == "maxPeSubsidy") {
    # Read max primary energy subsidy levels
    output <- readSource("REMIND_11Regi", subtype = "maxPeSubsidy")
    description <- paste0("maximum primary energy subsidy levels (in $/Gj) to ",
                          "provide plausible upper bound: 40$/barrel ~ 8 $/GJ")
    # using primary energy to weight the max subsidy levels
    weight <- calcOutput("PE", aggregate = FALSE)[, 2005, "PE (EJ/yr)"]
  } else if (subtype == "propFeSubsidy") {
    # Read proportional adjustment final energy subsidy levels
    output <- readSource("REMIND_11Regi", subtype = "propFeSubsidy")[, , c("fehoi")]
    getNames(output) <- c("fehos")
    description <- "subsidy proportional cap to avoid liquids increasing dramatically"
    # average weight
    weight <- new.magpie(getItems(output, dim = 1), getYears(output), getNames(output), fill = 1)
  }

  # convert data from $2005 to $2017
  output <- GDPuc::convertGDP(
    gdp = output,
    unit_in = "constant 2005 US$MER",
    unit_out = mrdrivers::toolGetUnitDollar(),
    replace_NAs = c("linear", "with_USA")
  )

  # Return tax convergence levels aggregated to selected REMIND regions
  return(list(x = output, weight = weight, unit = "$/GJ", description = description))
}
