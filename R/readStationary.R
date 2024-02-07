#' Load Stationary File as magclass object
#'
#' @return magclass object
#'
#' @author Antoine Levesque, Robin Hasse
#' @importFrom magclass read.magpie mselect as.magpie mbind add_dimension
readStationary <- function() {

  scenarios <- list(
    SSPs  = paste0("SSP", 1:5),
    SSP2s = paste0("SSP2", c("EU", "_lowEn",
                             paste0("EU_NAV_", c("act", "tec", "ele", "lce", "all")),
                             paste0("EU_CAMP_", c("weak", "strong")))),
    SDPs  = paste0("SDP", c("", "_EI", "_MC", "_RC")))

  data <- read.magpie("EDGE_TradMod.cs4r")
  data[is.na(data)] <- 0
  getSets(data) <- c("region", "year", "scenario", "item")

  data <- mbind(
    data,
    toolAddDimensions(
      x = mselect(data, scenario = "SSP2", collapseNames = TRUE),
      dimVals = scenarios$SSP2s,
      dimName = "scenario",
      dimCode = 3.1
    ),
    toolAddDimensions(
      x = mselect(data, scenario = "SSP1", collapseNames = TRUE),
      dimVals = scenarios$SDPs,
      dimName = "scenario",
      dimCode = 3.1)
    )

  if ("scenario" %in% getSets(data)) {
    data <- mselect(data, scenario = Reduce(c, scenarios))
  }

  return(data)
}
