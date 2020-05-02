#' Read EDGE
#' 
#' Read-in an EDGE csv file as magclass object
#' 
#' 
#' @return magpie object of EDGE
#' @author Antoine Levesque
#' @seealso \code{\link{readSource}}
#' @param subtype FE for final energy or Capital for capital projections
#'
#' @examples
#' \dontrun{ a <- readSource(type="EDGE")
#' }
#' @importFrom magclass read.magpie
readEDGE <- function(subtype = "FE_stationary") {

 if (subtype == "FE_stationary"){
  mstationary <- read.magpie("EDGE_TradMod.cs4r")
  mstationary[is.na(mstationary)] <- 0
  # use SSP1 data also for SSP1plus/SDP
  mstationary_SDP <- mstationary[,,"SSP1"]
  getNames(mstationary_SDP) <- gsub("SSP1","SDP",getNames(mstationary_SDP))
  mstationary <- mbind(mstationary,mstationary_SDP)
  getSets(mstationary) <- c("region", "year", "scenario", "item")
  
    mdata <- mstationary
 } else if (subtype == "FE_buildings") {
 
   mbuilding <- read.csv("EDGE_buildings_EDGE_EUR_ETP_CCoff.csv") 
   mbuilding <- as.magpie(mbuilding)
   # read in additional data for SSP1plus/SDP
   mbuilding_SDP <- read.csv("EDGE_buildings_EDGE_EUR_ETP_CCoff_SSP1plus.csv") 
   mbuilding_SDP <- as.magpie(mbuilding_SDP)
   # rename SSP1 into SDP
   getNames(mbuilding_SDP) <- gsub("SSP1plus","SDP",getNames(mbuilding_SDP))
   # add data on SSP1plus/SDP to data for all other scenarios
   mbuilding <- mbind(mbuilding,mbuilding_SDP)
   getSets(mbuilding) <- c("region", "year", "scenario", "item")
 
   mdata = mbuilding  
 } else if(subtype == "Capital"){
   mcapital = read.csv("capitalProjections.csv")
   mcapital = as.magpie(mcapital)
   mcapital = collapseNames(mcapital)
   getSets(mcapital) <- c("region", "year", "scenario")
   mdata = mcapital
   
 } else if(subtype == "CapitalUnit"){
   
   mcapitalunit_cap = read.csv("capitalUnitCost_cap.csv")
   mcapitalunit_cap$type = "cap"
   mcapitalunit_inv = read.csv("capitalUnitCost_inv.csv")
   mcapitalunit_inv$type = "inv"
   mcapitalunit = rbind(mcapitalunit_cap,mcapitalunit_inv)
   mcapitalunit = mcapitalunit[c(setdiff(colnames(mcapitalunit),"value"),"value")]
   mcapitalunit = as.magpie(mcapitalunit,tidy = T)
   mcapitalunit = collapseNames(mcapitalunit)
   mdata = mcapitalunit
   
 } else if(subtype == "Floorspace"){
   mfloor = read.csv("EDGE_buildings_floorspace.csv")
   mfloor = as.magpie(mfloor)
   mfloor = collapseNames(mfloor)
   getSets(mfloor) <- c("region", "year", "scenario")
   mdata = mfloor
 }else if(subtype == "ES_buildings"){
   mservices = read.csv("EDGE_buildings_services_EDGE_EUR_ETP.csv")
   mservices = as.magpie(mservices)
   mservices = collapseNames(mservices)
   getSets(mservices) <- c("region", "year", "scenario")
   mdata = mservices
 }else if(subtype == "FE_transport"){
   mtransport <- read.magpie("EDGE_transport_data.csv", file_type="csv")
   mtransport = collapseNames(mtransport)
   getSets(mtransport) <- c("region", "year", "scenario")
   mdata = mtransport
 }else { stop("valid subtypes are 'FE_stationary', 'FE_buildings', 'Floorspace','ES_buildings and 'Capital', 'FE_transport")} 
  return(mdata)
}  
