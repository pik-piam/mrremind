#' read-in power Australian gas extraction cost curve based on Dylan's data
#' Australian contact: Dylan McConnell, dylan.mcconnell(at)unimelb.edu.au
#' @return magpie object of the cemo database data
#' @author Felix Schreyer
#' @seealso \code{\link{readSource}}
#'
readDylanAusGasCost <- function() {
  "!# @monitor GDPuc::convertGDP"

  # Dylan's gas extraction cost curve based on GSOO data on reserves and resources in 2015
  # (only for Eastern gas reserves, not Western Australia!!)

  GasData <- read.csv("GSOO-costcurve.csv", stringsAsFactors = FALSE)

  GasData$country <- "AUS"
  GasData$year <- "y2005"
  GasData$PRICE[GasData$PRICE == "-"] <- ""
  GasData$PRICE <- as.numeric(GasData$PRICE)
  GasData$ID <- 1:dim(GasData)[1]

  GasData <- GasData[, c(6, 7, 8, 3, 4, 5)]

  x <- as.magpie(GasData, spatial = 1, temporal = 2, datacol = 6)

  return(x)
}
