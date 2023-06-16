#' @title calc transport share in GDP
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
#' @importFrom magclass add_dimension
#'

calcTransportGDPshare <- function() {


  ggdc10 <- readSource(type = "GGDC10") # Read GGDC 10-Sector Database

  #transport share in GDP
  share <- ggdc10[, c(2005, 2010), "VA_Q05.Transport, storage and communication"] / ggdc10[, c(2005, 2010), "VA_Q05.Summation of sector GDP"]
  share[is.na(share)] <- 0

  #Setting 2010 missing data equal to 2005
  for (country in getItems(share, dim = 1)) {
    if (share[country, 2010, ] == 0) {
      share[country, 2010, ] <- share[country, 2005, ]
    }
  }

  # calculating regional averages to fill missing data
  mapH12 <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", where = "mappingfolder")
  w <- ggdc10[, c(2005, 2010), "VA_Q05.Summation of sector GDP"]
  aggData <- toolAggregate(share, mapH12, weight = w)
  aggData["EUR", 2010, ] <- aggData["EUR", 2005, ] # European countries are missing total 2010 values

  #Setting missing data to global average numbers
  mapGLO <- mapH12
  mapGLO$RegionCode <- "GLO"
  averageGLO <- toolAggregate(share, mapGLO, weight = w)
  aggData[c("NEU", "REF", "CAZ"), , ] <- averageGLO["GLO", , ]

  for (region in unique(mapH12$RegionCode)) {
    for (country in mapH12[mapH12$RegionCode == region, ]$CountryCode) {
      for (year in c(2005, 2010)) {
        if (all(share[country, year, ] == 0)) {
          share[country, year, ] <- aggData[region, year, ]
      }
      }
    }
  }

  share <- collapseDim(share)

  weight <- calcOutput("GDP", aggregate = FALSE, years = getYears(share, as.integer = TRUE))[, , "gdp_SSP2"]
  weight <- collapseDim(weight)

  #Returning capacity values
  return(list(x = share, weight = weight,
              unit = "percentage",
              description = "transport share in GDP"
  ))
}
