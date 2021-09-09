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
      
      # use SSP2 data also for SSP2Ariadne
      mstationary_SSP2Ariadne <- mstationary[,,"SSP2"]
      getNames(mstationary_SSP2Ariadne) <- gsub("SSP2", "SSP2Ariadne", getNames(mstationary_SSP2Ariadne))
      mstationary <- mbind(mstationary, mstationary_SSP2Ariadne)
      # use SSP1 data also for SDPs
      mstationary_SDP <- mstationary[,,"SSP1"]
      for (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
         getNames(mstationary_SDP) <- gsub("SSP1", i, getNames(mstationary[,,"SSP1"]))
         mstationary <- mbind(mstationary, mstationary_SDP)
      }
    
      getSets(mstationary) <- c("region", "year", "scenario", "item")
      return(mstationary)
    },
    FE_buildings = {
      mbuilding <- read.csv("EDGE_buildings_EDGE_EUR_ETP_CCoff.csv") 
      mbuilding <- as.magpie(mbuilding)
      # read in additional data for SDP (still called SSP1plus here...)
      mbuilding_SSP1plus <- read.csv("EDGE_buildings_EDGE_EUR_ETP_CCoff_SSP1plus.csv") %>%
         as.magpie()
      
      mbuilding_SDP <- mbuilding_SSP1plus
      for (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
        getNames(mbuilding_SDP) <- gsub("SSP1plus", i, getNames(mbuilding_SSP1plus))
        mbuilding <- mbind(mbuilding, mbuilding_SDP)
      }   
      # use SSP2 data also for SSP2Ariadne
      mbuilding_SSP2Ariadne <- mbuilding[,,"SSP2"]
      getNames(mbuilding_SSP2Ariadne) <- gsub("SSP2", "SSP2Ariadne", getNames(mbuilding_SSP2Ariadne))
      mbuilding <- mbind(mbuilding, mbuilding_SSP2Ariadne)
      
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
