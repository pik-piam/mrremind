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
#' @author Antoine Levesque, Robin Hasse
#' @seealso \code{\link{readSource}}
#' @importFrom magclass read.magpie mselect as.magpie mbind add_dimension
readEDGE <- function(subtype = c("FE_stationary", "FE_buildings", "Capital", "CapitalUnit", "Floorspace",
                                 "ES_buildings")) {
  subtype <- match.arg(subtype)

  # input data version
  ver <- "1.12"
  scenarios <- list(
    SSPs  = paste0("SSP", 1:5),
    SSP2s = paste0("SSP2", c("EU", "_lowEn",
                             paste0("EU_NAV_", c("act", "tec", "ele", "lce", "all")),
                             paste0("EU_CAMP_", c("weak", "strong")))),
    SDPs  = paste0("SDP", c("", "_EI", "_MC", "_RC")))

  addDim <- function(x, addnm, dim, dimCode = 3.2) {
    do.call("mbind", lapply(addnm, function(item) {
      add_dimension(x, dim = dimCode, add = dim, nm = item)
    }))
  }

  switch(subtype,
    # TODO: what is this about and can this be moved somewhere else?
    FE_stationary = {
      data <- read.magpie(file.path(ver, "EDGE_TradMod.cs4r"))
      data[is.na(data)] <- 0
      getSets(data) <- c("region", "year", "scenario", "item")
      data <- mbind(
        data,
        addDim(mselect(data, scenario = "SSP2", collapseNames = TRUE),
               scenarios$SSP2s, "scenario", 3.1),
        addDim(mselect(data, scenario = "SSP1", collapseNames = TRUE),
               scenarios$SDPs, "scenario", 3.1))},
    FE_buildings = {
      data <- read.csv(file.path(ver, "EDGE_buildings_energy.csv"))
      data <- as.magpie(data)
      getNames(data) <- gsub("rcp", "", getNames(data))
      getNames(data) <- gsub("NoC", "fixed", getNames(data))
      getSets(data) <- c("region", "year", "scenario", "rcp", "item")},
    Capital = {
      data <- read.csv(file.path(ver, "capitalProjections.csv"))
      data <- as.magpie(data)
      data <- collapseNames(data)
      getItems(data, 3.1) <- sub("gdp_", "", getItems(data, 3.1))
      getSets(data) <- c("region", "year", "scenario", "variable")},
    CapitalUnit = {
      mcapitalunitCap <- read.csv(file.path(ver, "capitalUnitCost_cap.csv"))
      mcapitalunitCap$type <- "cap"
      mcapitalunitInv <- read.csv(file.path(ver, "capitalUnitCost_inv.csv"))
      mcapitalunitInv$type <- "inv"
      data <- rbind(mcapitalunitCap, mcapitalunitInv)
      data <- data[c(setdiff(colnames(data), "value"), "value")]
      data <- as.magpie(data, tidy = TRUE)
      data <- collapseNames(data)},
    Floorspace = {
      data <- read.csv(file.path(ver, "EDGE_buildings_floorspace.csv"))
      data <- as.magpie(data)
      data <- collapseNames(data)
      getSets(data) <- c("region", "year", "scenario", "variable")},
    ES_buildings = {
      data <- read.csv(file.path(ver, "EDGE_buildings_service.csv"))
      data <- as.magpie(data)
      # Only consider trajectories with fixed climate for services
      data <- mselect(data, rcp = "rcpNoC", collapseNames = TRUE)
      getSets(data) <- c("region", "year", "scenario", "item")}
  )

  if ("scenario" %in% getSets(data)) {
    data <- mselect(data, scenario = Reduce(c, scenarios))
  }

  return(data)
}
