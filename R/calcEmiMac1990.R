#' Calculate baseline emissions for maccs for 1990
#'
#' Provides REMIND data for baseline emissions for maccs for 1990.
#'
#'
#' @return REMIND data for baseline emissions for maccs for 1990 and
#' corresonding weights (NULL) as a list of two MAgPIE objects
#' @author Lavinia Baumstark
#' @examples
#' \dontrun{
#' calcOutput("EmiMac1990")
#' }
calcEmiMac1990 <- function() {
  # emissions for the calculation of econometric paramter p1
  ch4 <- readSource("EDGAR", subtype = "ch4_history") * 1e-3

  ch4wsts <- dimSums(ch4[, 1990, c("6B")], dim = 3)
  getNames(ch4wsts) <- "ch4wsts"
  ch4wstl <- dimSums(ch4[, 1990, c("6A", "6C", "6D")], dim = 3)
  getNames(ch4wstl) <- "ch4wstl"

  # overwritting european countries with eurostat data
  EUcountries <- c("ALA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "GGY", "HUN", "IRL", "IMN", "ITA", "JEY", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
  baselineEurostat <- readSource(type = "Eurostat", subtype = "MACCemi")
  gwp_ch4_eurostat <- 25 # values from AR4
  ch4wsts[EUcountries, 1990, "ch4wsts"] <- baselineEurostat[EUcountries, 1990, "ch4wsts"] / gwp_ch4_eurostat
  ch4wstl[EUcountries, 1990, "ch4wstl"] <- baselineEurostat[EUcountries, 1990, "ch4wstl"] / gwp_ch4_eurostat

  # combine all parameters
  x <- mbind(ch4wsts, ch4wstl)
  getYears(x) <- NULL
  return(list(
    x = x,
    weight = NULL,
    unit = "MtCH4",
    description = "emissions in 1990",
    note = c("used to calculate econometric emission parameter p1")
  ))
}
