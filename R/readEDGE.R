#' Read EDGE
#'
#' Load an EDGE Buildings file as magclass object.
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("EDGE")
#' }
#' @author Antoine Levesque
#' @seealso \code{\link{readSource}}
#' @importFrom magclass read.magpie
readEDGE <- function(subtype = c("FE_stationary", "FE_buildings", "Capital", "CapitalUnit", "Floorspace",
                                 "ES_buildings")) {
  subtype <- match.arg(subtype)
  switch(
    subtype,
    FE_stationary = {
      mstationary <- read.magpie("EDGE_TradMod.cs4r")
      mstationary[is.na(mstationary)] <- 0
      # use SSP1 data also for SSP1plus/SDP
      mstationarySDP <- mstationary[, , "SSP1"]
      getNames(mstationarySDP) <- gsub("SSP1", "SDP", getNames(mstationarySDP))
      mstationary <- mbind(mstationary, mstationarySDP)
      getSets(mstationary) <- c("region", "year", "scenario", "item")
      return(mstationary)
    },
    FE_buildings = {
      mbuilding <- read.csv("EDGE_buildings_EDGE_EUR_ETP_CCoff.csv")
      mbuilding <- as.magpie(mbuilding)
      # read in additional data for SSP1plus/SDP
      mbuildingSDP <- read.csv("EDGE_buildings_EDGE_EUR_ETP_CCoff_SSP1plus.csv")
      mbuildingSDP <- as.magpie(mbuildingSDP)
      # rename SSP1plus to SDP
      getNames(mbuildingSDP) <- gsub("SSP1plus", "SDP", getNames(mbuildingSDP))
      # add data on SSP1plus/SDP to data for all other scenarios
      mbuilding <- mbind(mbuilding, mbuildingSDP)
      getSets(mbuilding) <- c("region", "year", "scenario", "item")
      return(mbuilding)
    },
    Capital = {
      mcapital <- read.csv("capitalProjections.csv")
      mcapital <- as.magpie(mcapital)
      mcapital <- collapseNames(mcapital)
      getSets(mcapital) <- c("region", "year", "scenario")
      return(mcapital)
    },
    CapitalUnit = {
      mcapitalunitCap <- read.csv("capitalUnitCost_cap.csv")
      mcapitalunitCap$type <- "cap"
      mcapitalunitInv <- read.csv("capitalUnitCost_inv.csv")
      mcapitalunitInv$type <- "inv"
      mcapitalunit <- rbind(mcapitalunitCap, mcapitalunitInv)
      mcapitalunit <- mcapitalunit[c(setdiff(colnames(mcapitalunit), "value"), "value")]
      mcapitalunit <- as.magpie(mcapitalunit, tidy = TRUE)
      mcapitalunit <- collapseNames(mcapitalunit)
      return(mcapitalunit)
    },
    Floorspace = {
      mfloor <- read.csv("EDGE_buildings_floorspace.csv")
      mfloor <- as.magpie(mfloor)
      mfloor <- collapseNames(mfloor)
      getSets(mfloor) <- c("region", "year", "scenario", "variable")
      return(mfloor)
    },
    ES_buildings = {
      mservices <- read.csv("EDGE_buildings_services_EDGE_EUR_ETP.csv")
      mservices <- as.magpie(mservices)
      mservices <- collapseNames(mservices)
      getSets(mservices) <- c("region", "year", "scenario")
      return(mservices)
    }
  )
}
