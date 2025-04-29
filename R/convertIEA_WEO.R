#' Converts IEA World Energy Outlook data
#'
#' @param x MAgPIE object to be converted
#' @param subtype data subtype. Either "Capacity", "Generation", "Emissions",
#' "Investment Costs", or "O&M Costs"
#' @return magpie object of the WEO data on generation (TWh), capacities (GW),
#' emissions (Mt CO2) or disaggregated investment cost as magpie object
#' @author Renato Rodrigues and Aman Malik
#'
#' @examples
#' \dontrun{
#' a <- convertWEO(x, subtype = "Capacity")
#' }
#'
#' @importFrom readxl read_excel
convertIEA_WEO <- function(x, subtype) {

  if (subtype == "Invest_Costs" || subtype == "O&M_Costs") {

    # mapping of all countries and their respective regions
    all_c <- toolGetMapping("regionmappingH12.csv", where = "mappingfolder", type = "regional")
    # seperating hydro case
    x_hydro <- x[unique(all_c$RegionCode), , "Hydro_2.hydro"]
    x <- x[unique(all_c$RegionCode), , "Hydro_2.hydro", invert = TRUE]
    eu_costs <- new.magpie(all_c$CountryCode[all_c$RegionCode == "EUR"], years = getYears(x), names = getNames(x))
    neu_costs <- new.magpie(all_c$CountryCode[all_c$RegionCode == "NEU"], years = getYears(x), names = getNames(x))
    africa_costs <- new.magpie(all_c$CountryCode[all_c$RegionCode == "SSA"], years = getYears(x), names = getNames(x))
    mea_costs <- new.magpie(all_c$CountryCode[all_c$RegionCode == "MEA"], years = getYears(x), names = getNames(x))
    asia_costs <- new.magpie(all_c$CountryCode[all_c$RegionCode == "OAS"], years = getYears(x), names = getNames(x))
    caz_costs <- new.magpie(all_c$CountryCode[all_c$RegionCode == "CAZ"], years = getYears(x), names = getNames(x))
    sc_america_costs <- new.magpie(all_c$CountryCode[all_c$RegionCode == "LAM"], years = getYears(x), names = getNames(x))

    # All countries from the database which have zero investment costs get some numbers
    # So, for e.g., Brazil and Middle East have zero for all the CCS related investment costs. They get their value from the
    # average of all other regions/countries for the same technology.
    x[c("Brazil", "Middle East"), , "CCS", pmatch = TRUE] <- dimSums(x[c("Brazil", "Middle East"), , invert = TRUE][, , "CCS"], na.rm = TRUE, dim = 1) /
      nregions(x[c("Brazil", "Middle East"), , invert = TRUE])
    x["Brazil", , "Renewables.Geothermal"] <- dimSums(x["Brazil", , , invert = TRUE][, , "Renewables.Geothermal"], dim = 1) /
      nregions(x["Brazil", , invert = TRUE])
    x["Brazil", , "Renewables.Marine"] <- dimSums(x["Brazil", , , invert = TRUE][, , "Renewables.Marine"], dim = 1) /
      nregions(x["Brazil", , invert = TRUE])
    x[c("Japan", "Russia"), , "Renewables.Concentrating solar power"] <- dimSums(x[c("Japan", "Russia"), , , invert = TRUE][, , "Renewables.Concentrating solar power"], dim = 1) /
      nregions(x[c("Japan", "Russia"), , invert = TRUE])
    for (n in getItems(eu_costs, dim = 1)) { # all EU28 countries get "Europe" investment costs
      eu_costs[n, , ] <- as.numeric(x["Europe", , ])
    }
    for (n in getItems(neu_costs, dim = 1)) { # all non-EU european countries get average of "Europe" and "RUS" investment costs
      neu_costs[n, , ] <- 0.5 * as.numeric(x["Europe", , ]) + 0.5 * as.numeric(x["Russia", , ])
    }
    for (n in getItems(caz_costs, dim = 1)) { # all CAZ countries get "OECD" average investment costs
      caz_costs[n, , ] <- 0.34 * as.numeric(x["Europe", , ]) + 0.33 * as.numeric(x["United States", , ]) + 0.33 * as.numeric(x["Japan", , ])
    }
    for (n in getItems(africa_costs, dim = 1)) { # all african countries get "Africa" investment costs
      africa_costs[n, , ] <- as.numeric(x["Africa", , ])
    }
    for (n in getItems(mea_costs, dim = 1)) { # all OAS countries get average of Indian and Japanese investment costs
      mea_costs[n, , ] <- as.numeric(x["Middle East", , ])
    }
    for (n in getItems(asia_costs, dim = 1)) { # all OAS countries get average of Indian and Japanese investment costs
      asia_costs[n, , ] <- 0.5 * as.numeric(x["India", , ]) + 0.5 * as.numeric(x["Japan", , ])
    }

    for (n in getItems(sc_america_costs, dim = 1)) {
      sc_america_costs[n, , ] <- as.numeric(x["Brazil", , ]) # all S. and C. American countries get "Brazilian" investment costs
    }

    # Extracting proper countries from the region/country list in input data
    x_countries <- x[c("Europe", "Middle East", "Africa"), , , invert = TRUE]

    # converting to ISO country names
    getItems(x_countries, dim = 1) <- toolCountry2isocode(getItems(x_countries, dim = 1))

    # one magpie object with all names
    x_total <- mbind(
      eu_costs, neu_costs, caz_costs, africa_costs, mea_costs, asia_costs,
      sc_america_costs, x_countries[c("BRA"), , invert = TRUE]
    )
    # remaining countries
    x_total <- toolCountryFill(x_total, verbosity = 2, "HKG" = "CHN", "PRI" = "USA", "MAC" = "CHN")

    x_hydro <- toolAggregate(x_hydro, rel = all_c, from = "RegionCode", to = "CountryCode")

    # setting 2040 values as 2030 values
    x_hydro[, "y2040", ] <- setYears(x_hydro[, 2030, ])

    x_total[, , "Renewables.Hydropower - large-scale"] <- x_hydro[, , "Hydro_2.hydro"]

    x <- x_total

    x[is.na(x)] <- 0
  } else if ((subtype == "Capacity") || (subtype == "Generation") || (subtype == "Emissions")) {

    H12map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")

    if (subtype == "Capacity") { # estimate OAS coal

      # Approximate Caspian countries (part of the REF)
      weight <- calcOutput("IO", subtype = "input", aggregate = FALSE)[, 2015, "pecoal.seel.pc"]
      caspian <- new.magpie(cells_and_regions = "Caspian", names = getNames(x), years = 2015, fill = as.vector(x["EURASIA", 2015, getNames(x)]) - as.vector(x["RUS", 2015, getNames(x)]))
      caspianMap <- data.frame(CountryCode = c("ARM", "AZE", "GEO", "KAZ", "KGZ", "TJK", "TKM", "UZB"), RegionCode = "Caspian")
      caspianCountries <- toolAggregate(caspian, caspianMap, weight[c("ARM", "AZE", "GEO", "KAZ", "KGZ", "TJK", "TKM", "UZB"), , ])

      # Approximate Latin America countries (minus MEX)
      LAM <- new.magpie(cells_and_regions = "LAM", names = getNames(x), years = 2015, fill = as.vector(x["CSAM", 2015, getNames(x)]) - as.vector(x["BRAZIL", 2015, getNames(x)]))
      LAMCountryCode <- H12map[which(H12map$RegionCode == "LAM"), ]$CountryCode
      LAMCountries <- toolAggregate(LAM, H12map[which(H12map$CountryCode %in% LAMCountryCode[!LAMCountryCode %in% c("MEX", "BRA")]), ], weight[LAMCountryCode[!LAMCountryCode %in% c("MEX", "BRA")], , ])

      # Approximate OAS countries
      AUS_Coal <- 20 # Australia coal capacity in 2015 (GW)
      NZL_Coal <- 0.55 # New zealand coal capacity in 2015 (GW)
      OAStotal <- as.vector(x["ASIAPAC", 2015, "Coal"]) - as.vector(x["CHINA", 2015, "Coal"]) - as.vector(x["INDIA", 2015, "Coal"]) - as.vector(x["JPN", 2015, "Coal"]) - AUS_Coal - NZL_Coal
      OAS <- new.magpie(cells_and_regions = "OAS", names = c("Coal"), years = 2015, fill = OAStotal)
      OASCountries <- toolAggregate(OAS, H12map[which(H12map$RegionCode == "OAS"), ], weight[H12map[which(H12map$RegionCode == "OAS"), ]$CountryCode, , ])
    }

    # get other regions directly from WEO data
    otherCap <- x[c("US", "BRAZIL", "RUS", "CHINA", "INDIA", "JPN"), 2015, ] # countries only
    getItems(otherCap, dim = 1) <- toolCountry2isocode(getItems(otherCap, dim = 1), mapping = c("US" = "USA", "RUS" = "RUS", "JPN" = "JPN"))

    # merge data
    if (subtype == "Capacity") {
      reg <- unique(c(getItems(otherCap, dim = 1), getItems(OASCountries, dim = 1), getItems(caspianCountries, dim = 1), getItems(LAMCountries, dim = 1), "MEX"))
      names <- unique(c(getNames(otherCap), getNames(OASCountries), getNames(caspianCountries), getNames(LAMCountries)))
      years <- unique(c(getYears(otherCap), getYears(OASCountries), getYears(caspianCountries), getYears(LAMCountries)))
      out <- new.magpie(cells_and_regions = reg, names = names, years = years)
      out[getItems(otherCap, dim = 1), , ] <- otherCap
      out[getItems(OASCountries, dim = 1), , "Coal"] <- OASCountries[, , "Coal"]
      out[getItems(caspianCountries, dim = 1), , "Coal"] <- caspianCountries[, , "Coal"]
      out[getItems(LAMCountries, dim = 1), , "Coal"] <- LAMCountries[, , "Coal"]
      out["MEX", , "Coal"] <- 5.378 # Mexico coal capacity in 2015 (GW)
    } else {
      out <- otherCap
    }

    # fill countries with no data
    out <- toolCountryFill(out, fill = 0, verbosity = 2)

    # replace NAs with 0
    out[is.na(out)] <- 0

    x <- out
  }

  if ((subtype == "PE") || (subtype == "FE")) {
    x <- collapseNames(x)

    regions <- c(
      "BRAZIL", "CHINA", "JPN",
      "INDIA",
      "US", "RUS", "SAFR"
    )
    y <- x[regions, , ]

    getItems(y, dim = 1) <- gsub("JPN", "Japan", getItems(y, dim = 1))
    getItems(y, dim = 1) <- gsub("RUS", "Russia", getItems(y, dim = 1))
    getItems(y, dim = 1) <- gsub("US", "United States of America", getItems(y, dim = 1))
    getItems(y, dim = 1) <- gsub("SAFR", "South Africa", getItems(y, dim = 1))

    y <- mbind(y)

    getItems(y, dim = 1) <- toolCountry2isocode(getItems(y, dim = 1))
    x <- y
    x <- toolCountryFill(x = y, fill = NA, verbosity = 2)

    if (subtype == "PE") {
      x <- x[, , "Primary Energy", pmatch = TRUE]
    }

    if (subtype == "FE") {
      x <- x[, , "Final Energy", pmatch = TRUE]
    }
  }
  return(x)
}
