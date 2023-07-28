#' Convert EU Reference Scenario
#'
#' Converts EU Reference Scenario magpie object into appropriate form for the REMIND model
#'
#' @author Renato Rodrigues, Falk Benke, Robin Hasse
#' @param x EU Reference Scenario magpie object derived from readEU_ReferenceScenario function
#' @param subtype data subtype. Either "techAssump.*", "2016" or "2020"
#' @return converted EU Reference Scenario magpie object
#' @examples
#' \dontrun{
#' test <- readSource("EU_ReferenceScenario", subtype = "2020", convert = TRUE)
#' }
#' @importFrom madrat toolGetMapping toolCountryFill toolAggregate
#' @importFrom magclass getItems<- getItems getSets<- setItems mselect
#' @importFrom utils read.csv2
#' @export

convertEU_ReferenceScenario <- function(x, subtype) {

  EU_27 <- c(
    "ALA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
    "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "GGY", "HUN", "IRL",
    "IMN", "ITA", "JEY", "LVA", "LTU", "LUX", "MLT", "NLD", "POL",
    "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
  )
  EU_28 <- c(EU_27, "GBR")



  # Technology assumptions -----------------------------------------------------

  if (grepl("^techAssump\\..+$", subtype)) {

    subsubtype <- sub("^techAssump\\.", "", subtype)

    # map to EU 28 countries
    mapping <- toolGetMapping(name = "regionmappingEU_ReferenceScenario.csv",
                              type = "regional",
                              where = "mappingfolder")
    mapping <- mapping[mapping[["CountryCode"]] %in% EU_28, ]
    if (subsubtype %in% c("Domestic", "Renovation Costs")) {
      xReg <- x["EUR", , invert = TRUE]
      xReg <- toolAggregate(x = xReg,
                            rel = mapping,
                            from = gsub(" ", ".", subsubtype),
                            to = "CountryCode")
      if (subsubtype == "Renovation Costs") {
        x <- xReg
      } else {
        xEur <- mselect(x, region = "EUR")
        xEur <- do.call(mbind, lapply(EU_28, setItems, x = xEur, dim = 1))
        x <- xEur
        x[!is.na(xReg)] <- xReg[!is.na(xReg)]
      }
    } else {
      x <- do.call(mbind, lapply(EU_28, setItems, x = x, dim = 1))
      getSets(x)[1] <- "region"
    }

    # harmonise units
    getItems(x, "unit") <- sub("Euro", "EUR", getItems(x, "unit"))
    getItems(x, "unit") <- sub("^NA$", "EUR/appliance", getItems(x, "unit"))

  } else {

    # results ------------------------------------------------------------------

    # fill up zero countries
    iso3 <- read.csv2("isotwo2iso3Mapping.csv", stringsAsFactors = FALSE)
    getItems(x, dim = 1) <- sapply(getItems(x, dim = 1), function(y) iso3[which(iso3[, 1] == y), 2])

  }

  x <- toolCountryFill(x, fill = NA, verbosity = 2)

  EU <- switch(subtype, "2016" = EU_28, "2020" = EU_27)

  x.eu <- x[EU, , ]
  x.eu[is.na(x.eu)] <- 0
  x[EU, , ] <- x.eu[EU, , ]

  return(x)
}
