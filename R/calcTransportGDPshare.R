#' @title Get transport share in GDP
#' @description provides transport share in GDP to resize edge-t transportation purchase costs
#'
#' @return magpie object of transport shares in GDP
#' @author Renato Rodrigues
#' @examples
#'
#' \dontrun{
#' calcOutput("TransportGDPshare")
#' }
#'
calcTransportGDPshare <- function() {

  vars <- c("VA_Q05.Transport, storage and communication", "VA_Q05.Summation of sector GDP")
  years <- c(2005, 2010)
  ggdc10 <- readSource("GGDC10")[, years, vars]

  # Setting missing 2010 data equal to 2005 if available
  for (i in getItems(ggdc10, dim = 1)) {
    for (j in getItems(ggdc10, dim = 3)) {
      if (ggdc10[i, 2010, j] == 0) ggdc10[i, 2010, j] <- ggdc10[i, 2005, j]
    }
  }

  share <- ggdc10[, , "VA_Q05.Transport, storage and communication"] /
    ggdc10[, , "VA_Q05.Summation of sector GDP"]

  # Fill missing data with regional, and then global averages
  mapH12 <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", where = "mappingfolder")
  mapGLO <- mapH12
  mapGLO$RegionCode <- "GLO"
  w <- ggdc10[, , "VA_Q05.Summation of sector GDP"]
  share <- share %>%
    toolFillWithRegionAvg(valueToReplace = 0, weight = w, regionmapping = mapH12, warningThreshold = 1) %>%
    toolFillWithRegionAvg(weight = w, regionmapping = mapGLO, warningThreshold = 1)

  share <- collapseDim(share)
  weight <- calcOutput("GDP", scenario = "SSP2", aggregate = FALSE, years = years)
  weight <- collapseDim(weight)

  list(x = share, weight = weight, unit = "percentage", description = "transport share in GDP")
}
