#' Calculate SubsStationary
#'
#' Reads in the data of the source IIASA_subs_taxes, by country. and
#' calculate taxes at primary energy level. Regional aggregation is done via the
#' respective energy quantities as weights.
#'
#' @param subtype subsidies rate ("subsidies") output
#'
#' @return MAgPIE object
#' @author Christoph Bertram and Renato Rodrigues
#' @seealso \code{\link{readIIASA_subs_taxes}},
#' \code{\link{convertIIASA_subs_taxes}}
#' @examples
#' \dontrun{
#' calcOutput("PETaxes")
#' }
#'
calcPETaxes <- function(subtype = "subsidies") {
  if (subtype != "subsidies") {
    stop("the subtype must be 'subsidies'")
  }

  tax <- -readSource("IIASA_subs_taxes", subtype = "subsidies_bulk")
  desc <- "Aggregated primary energy subsidy data from country level data provided by IIASA (Jessica Jewell)"

  # read in energy values
  energy <- readSource("IIASA_subs_taxes", subtype = "energy")
  # energy = 0 for regions/carriers with no information on subsidies, so that they are not considered in the weighting
  energy[is.na(tax)] <- 0
  # taxes without value are considered to be zero
  tax[is.na(tax)] <- 0
  # energy without value is considered to be zero
  energy[is.na(energy)] <- 0

  tax_map <- c(
    "pegas" = "PE-Naturalgas",
    "peoil" = "PE-Oil",
    "pecoal" = "PE-Coal"
  )

  Rtax <- setNames(tax[, , tax_map], names(tax_map))
  Renergy <- setNames(energy[, , tax_map], names(tax_map))

  # convert original data from bulk values to subsidies rates for the case of subsidies
  Rtax <- Rtax / Renergy * 1e9 # converting from billion$/GJ to $/GJ
  Rtax[is.na(Rtax)] <- 0

  # avoid zero weights, as they cause a warning in aggregation
  Renergy[Renergy == 0] <- 1e-10

  # set base year
  getYears(Rtax) <- "2005"
  getYears(Renergy) <- "2005"

  return(list(x = Rtax, weight = Renergy, unit = "US$2017/GJ", description = desc))
}
