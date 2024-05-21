#' Read EU Effort Sharing targets and historical emissions
#'
#' Read-in EU Effort Sharing targets and historical emissions csv files as magclass object
#'
#' @param subtype data subtype. Either "target" or "emissions"
#' @return magpie object of the EU Effort Sharing targets (%) or Effort Sharing historical historical emissions (MtCO2)
#' @author Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "Eurostat_EffortSharing", subtype = "target")
#' }
#'
readEurostat_EffortSharing <- function(subtype) {
  if (subtype == "target") {
    # Reading EU Effort Sharing country targets
    data <- read.csv("effort_sharing_targets.csv", sep = ";", skip = 5)
    colnames(data) <- c("iso2", "country.name", "iso3", "2020", "2030",
                        "flexibility_from_ETS_in_2030", "flexibility_from_land_use_in_2030")
    data <- na.omit(data[, c("iso3", "2020", "2030")])
  } else if (subtype == "emissions") {
    # Reading EU Effort Sharing country historical emissions
    data <- read.csv("t2020_35.csv", sep = ";")
    colnames(data) <- c("indic_eu", "iso2", "2005", "2006", "2007", "2008", "2009", "2010", "2011",
                        "2012", "2013", "2014", "2015", "2016", "2017", "target")
    iso2toiso3 <- c(
      "LU" = "LUX", "SE" = "SWE", "DK" = "DNK", "FI" = "FIN", "DE" = "DEU",
      "FR" = "FRA", "UK" = "GBR", "NL" = "NLD", "AT" = "AUT", "BE" = "BEL",
      "IT" = "ITA", "IE" = "IRL", "ES" = "ESP", "CY" = "CYP", "MT" = "MLT",
      "PT" = "PRT", "EL" = "GRC", "SI" = "SVN", "CZ" = "CZE", "EE" = "EST",
      "SK" = "SVK", "LT" = "LTU", "PL" = "POL", "HR" = "HRV", "HU" = "HUN",
      "LV" = "LVA", "RO" = "ROU", "BG" = "BGR"
    )
    data$iso3 <- iso2toiso3[as.character(data$iso2)]
    data <- na.omit(data[, c("iso3", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
                             "2013", "2014", "2015", "2016")])
  } else {
    stop("Not a valid subtype!")
  }

  # data in wide format
  data <- reshape2::melt(data, id.vars = c("iso3"), variable.name = "period", value.name = "value")

  # converting to magpie object
  as.magpie(data, spatial = 1, temporal = 2)
}
