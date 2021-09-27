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
#' @author Antoine Levesque, Robin Krekeler
#' @seealso \code{\link{readSource}}
#' @importFrom magclass read.magpie mselect as.magpie mbind add_dimension
readEDGE <- function(subtype = c("FE_stationary", "FE_buildings", "Capital", "CapitalUnit", "Floorspace",
                                 "ES_buildings")) {
  subtype <- match.arg(subtype)

  # input data version
  ver <- "1.03"

  addDim <- function(x, addnm, dim, dimCode = 3.2) {
    do.call("mbind", lapply(addnm, function(item) {
      add_dimension(x, dim = dimCode, add = dim, nm = item)
    }))
  }

  switch(subtype,
    FE_stationary = {
      mstationary <- read.magpie(file.path(ver, "EDGE_TradMod.cs4r"))
      mstationary[is.na(mstationary)] <- 0
      getSets(mstationary) <- c("region", "year", "scenario", "item")
      # duplicate: SSP2 -> SSP2EU, SSP2_lowEn and SSP1 -> SDPs
      mstationarySPP2s <- addDim(
        mselect(mstationary, scenario = "SSP2", collapseNames = TRUE),
        c("SSP2EU", "SSP2_lowEn"), "scenario", 3.1)
      mstationarySDPs <- addDim(
        mselect(mstationary, scenario = "SSP1", collapseNames = TRUE),
        c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"), "scenario", 3.1)
      mstationary <- mbind(mstationary, mstationarySPP2s, mstationarySDPs)
      return(mstationary)
    },
    FE_buildings = {
      mbuilding <- read.csv(file.path(ver, "EDGE_buildings_energy.csv"))
      mbuilding <- as.magpie(mbuilding)
      getNames(mbuilding) <- gsub("rcp", "", getNames(mbuilding))
      getNames(mbuilding) <- gsub("NoC", "fixed", getNames(mbuilding))
      getSets(mbuilding) <- c("region", "year", "scenario", "rcp", "item")
      # duplicate SDP scenario for SDP variants
      mbuildingSDPs <- addDim(
        mselect(mbuilding, scenario = "SDP", collapseNames = TRUE),
        c("SDP_EI", "SDP_RC", "SDP_MC"), "scenario", 3.1)
      mbuilding <- mbind(mbuilding, mbuildingSDPs)
      return(mbuilding)
    },
    Capital = {
      mcapital <- read.csv(file.path(ver, "capitalProjections.csv"))
      mcapital <- as.magpie(mcapital)
      mcapital <- collapseNames(mcapital)
      getSets(mcapital) <- c("region", "year", "scenario")
      return(mcapital)
    },
    CapitalUnit = {
      mcapitalunitCap <- read.csv(file.path(ver, "capitalUnitCost_cap.csv"))
      mcapitalunitCap$type <- "cap"
      mcapitalunitInv <- read.csv(file.path(ver, "capitalUnitCost_inv.csv"))
      mcapitalunitInv$type <- "inv"
      mcapitalunit <- rbind(mcapitalunitCap, mcapitalunitInv)
      mcapitalunit <- mcapitalunit[c(setdiff(colnames(mcapitalunit), "value"), "value")]
      mcapitalunit <- as.magpie(mcapitalunit, tidy = TRUE)
      mcapitalunit <- collapseNames(mcapitalunit)
      return(mcapitalunit)
    },
    Floorspace = {
      mfloor <- read.csv(file.path(ver, "EDGE_buildings_floorspace.csv"))
      mfloor <- as.magpie(mfloor)
      mfloor <- collapseNames(mfloor)
      getSets(mfloor) <- c("region", "year", "scenario", "variable")
      return(mfloor)
    },
    ES_buildings = {
      mservices <- read.csv(file.path(ver, "EDGE_buildings_service.csv"))
      mservices <- as.magpie(mservices)
      # Only consider trajectories with fixed climate for services
      mservices <- mselect(mservices, rcp = "rcpNoC", collapseNames = TRUE)
      getSets(mservices) <- c("region", "year", "scenario", "item")
      return(mservices)
    }
  )
}
