#' Calculate Cooling Type Shares
#'
#' This function merges the output of two other functions that calculate REMIND
#' input data for the shares of cooling types per electricity technology and
#' REMIND region, using as initial information the Davies (2013) data per
#' electricity technology and GCAM region. The two other functions separately
#' calculate data for the base year and for future time steps. The source data
#' provide most required information but some assumptions on missing data are
#' also made.
#'
#'
#' @return MAgPIE object on cooling type shares per elecricity technology and
#' REMIND region
#' @author Ioanna Mouratiadou
#' @seealso \code{\link{calcOutput}}, \code{\link{readDaviesCooling}},
#' \code{\link{convertDaviesCooling}},
#' \code{\link{calcCoolingSharesBase}},\code{\link{calcCoolingSharesFuture}}
#' @examples
#' \dontrun{
#'
#' a <- calcOutput("CoolingSharesAll")
#' }
#'
calcCoolingSharesAll <- function() {
  cooloutputBase <- calcOutput("CoolingSharesBase", aggregate = FALSE)
  cooloutputFuture <- calcOutput("CoolingSharesFuture", aggregate = FALSE)

  # merge two datasets
  outputAll <- mbind(cooloutputBase, cooloutputFuture)

  # assign aggregation weight
  weight <- dimSums(calcOutput("IO", subtype = "output", aggregate = FALSE)[, 2010, c("feelb", "feeli")], dim = 3)

  # set weights to zero for countries that were not contained in the GCAM2ISO mapping
  weight["ALA", , ] <- 0
  weight["ATA", , ] <- 0
  weight["BES", , ] <- 0
  weight["BLM", , ] <- 0
  weight["CUW", , ] <- 0
  weight["GGY", , ] <- 0
  weight["IMN", , ] <- 0
  weight["JEY", , ] <- 0
  weight["MAF", , ] <- 0
  weight["PSE", , ] <- 0
  weight["SSD", , ] <- 0
  weight["SXM", , ] <- 0

  return(list(
    x = outputAll,
    weight = weight,
    unit = "% of cooling type technologies",
    description = c(
      "Cooling shares for different cooling technologies based on ",
      "Davies et al. (2013) publication and using electricity use weights (aggregated ",
      "based on IEA World Energy Balances, 2014) for regional mapping"
    )
  ))
}
