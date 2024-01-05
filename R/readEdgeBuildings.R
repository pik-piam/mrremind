#' Load an EDGE Buildings file as magclass object.
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @author Antoine Levesque, Robin Hasse
#' @importFrom magclass mselect as.magpie
readEdgeBuildings <- function(subtype = c("FE", "Floorspace")) {

  subtype <- match.arg(subtype)

  # input data version
  ver <- "1.12"

  scenarios <- list(
    SSPs  = paste0("SSP", 1:5),
    SSP2s = paste0("SSP2", c("EU", "_lowEn",
                             paste0("EU_NAV_", c("act", "tec", "ele", "lce", "all")),
                             paste0("EU_CAMP_", c("weak", "strong")))),
    SDPs  = paste0("SDP", c("", "_EI", "_MC", "_RC")))

  switch(subtype,
    FE = {
      data <- read.csv(file.path(ver, "EDGE_buildings_energy.csv"))
      data <- as.magpie(data)
      getNames(data) <- gsub("rcp", "", getNames(data))
      getNames(data) <- gsub("NoC", "fixed", getNames(data))
      getSets(data) <- c("region", "year", "scenario", "rcp", "item")
    },
    Floorspace = {
      data <- read.csv(file.path(ver, "EDGE_buildings_floorspace.csv"))
      data <- as.magpie(data)
      data <- collapseNames(data)
      getSets(data) <- c("region", "year", "scenario", "variable")
    }
  )

  if ("scenario" %in% getSets(data)) {
    data <- mselect(data, scenario = Reduce(c, scenarios))
  }

  return(data)
}
