#' Calculate GHG Emission Factors from GHG emission targets
#'
#' Emission targets are represented by a GHG Emission Factor, which is the quotient of total GHG
#' emissions in the target year divided by the CEDS GHG emissions in 2015
#'
#' @author Aman Malik, Christoph Bertram, Oliver Richters, Sophie Fuchs, Rahel Mandaroux, Falk Benke
#' @param x a magclass object with targets read in from NDC or NPI database
#' @param subtype Emissions_YYYY_cond or Emissions_YYYY_uncond
#' @param subset String, designating the GDP scenarios to use
#' @seealso [convertUNFCCC_NDC()], [convertNewClimate()]
toolCalcGhgFactor <- function(x, subtype, subset) {

  reductionData <- x

  # Reference Emissions from CEDS
  ghg <- calcOutput("EmiTargetReference", aggregate = FALSE)

  # Future GDP values
  gdp <- calcOutput("GDP", scenario = subset, aggregate = FALSE)

  # Define EU countries + Croatia for special treatment because of joint targets
  # GBR has its own targets starting from 2022
  EUR_NDC_countries <- c("POL", "CZE", "ROU", "BGR", "HUN", "SVK", "LTU", "EST", "SVN",
                         "LVA", "DEU", "FRA", "ITA", "ESP", "NLD", "BEL", "GRC", "AUT",
                         "PRT", "FIN", "SWE", "IRL", "DNK", "LUX", "CYP", "MLT", "JEY",
                         "FRO", "GIB", "GGY", "IMN", "HRV",
                         if (grepl("_20(18|19|20|21)_", subtype)) "GBR")

  allowedType <- c("GHG-Absolute", "GHG", "GHG/GDP", "CO2/GDP", "GHG-fixed-total", "GHG/CAP")

  # copy over EU target to EU countries
  reductionData <- add_columns(reductionData, addnm = EUR_NDC_countries, dim = 1, fill = NA)
  reductionData[EUR_NDC_countries, , ] <- reductionData["EUR", , ]
  reductionData <- reductionData["EUR", , , invert = TRUE]

  # Calculate GHG target emissions in Mt CO2eq in target year based on information in the NDC database
  ## Type describes how the target is formulated
  ## - "GHG-Absolute" - absolute GHG change compared to reference year or given BAU emissions
  ## - "GHG" - relative GHG change compared to reference year or given BAU emissions
  ## - "GHG/GDP" or "CO2/GDP" - given the GDP growth from reference year to target year,
  ##    the GHG emissions in reference year are assumed to grow proportionally to GDP,
  ##    minus the the relative target
  ## -  "GHG-fixed-total" - total GHG emissions in target year
  ## Reference Year
  ## - used to determine the reference year for GHG changes (and GDP growth)
  ## - takes CEDS emissions for reference year (if not available, use latest year in CEDS emissions)
  ## - if the column contains "BAU", use the value in "BAU_or_Reference_emissions_in_MtCO2e" as reference
  .calcGhgTarget <- function(data) {

    regi <- getItems(data, dim = 1)
    year <- getYears(data)
    ghgTarget <- NA

    if (allowedType[data[regi, year, "Type"]] == "GHG-Absolute") { # absolute GHG change

      if (data[regi, year, "Reference_Year"] == -1) {  # -1 if BAU;
        if (!is.na(data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"])) {
          # target + BAU emissions in sheet
          ghgTarget <- data[regi, year, conditional] +
            data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"]
        } else {
          message("For ", regi, " in ", year, ", reference year is BAU, but BAU Emissions are missing.")
        }
      } else { # then Reference_Year contains a year
        # target + historic GHG emissions from CEDS (best fit)
        histYear <- min(data[regi, year, "Reference_Year"], max(getYears(ghg, as.integer = TRUE)))
        if (data[regi, year, "Reference_Year"] > max(getYears(ghg, as.integer = TRUE))) {
          message(
            "For ", regi, " in ", year, ", reference year ", data[regi, year, "Reference_Year"][1],
            " is above ", max(getYears(ghg, as.integer = TRUE)), ", so we use the latter as reference year."
          )
        }

        ghgTarget <- data[regi, year, conditional] +
          setYears(ghg[regi, histYear, ], NULL)
      }
    } else if (allowedType[data[regi, year, "Type"]] == "GHG") { # relative GHG change

      if (data[regi, year, "Reference_Year"] == -1) {  # -1 if BAU.
        if (!is.na(data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"])) {
          # target * BAU emissions in sheet
          ghgTarget <- (1 + data[regi, year, conditional]) *
            data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"]
        } else {
          message("For ", regi, " in ", year, ", reference year is BAU, but BAU Emissions are missing.")
        }
      } else { # then Reference_Year contains a year

        # target * historic GHG emissions from CEDS (best fit)
        histYear <- min(data[regi, year, "Reference_Year"], max(getYears(ghg, as.integer = TRUE)))
        if (data[regi, year, "Reference_Year"] > max(getYears(ghg, as.integer = TRUE))) {
          message(
            "For ", regi, " in ", year, ", reference year ", data[regi, year, "Reference_Year"][1],
            " is above ", max(getYears(ghg, as.integer = TRUE)), ", so we use the latter as reference year."
          )
        }

        ghgTarget <- (1 + data[regi, year, conditional]) *
          setYears(ghg[regi, histYear, ], NULL)

      }
    } else if (allowedType[data[regi, year, "Type"]] %in% c("GHG/GDP", "CO2/GDP")) { # GHG/GDP or CO2/GDP

      if (data[regi, year, "Reference_Year"] > max(getYears(ghg, as.integer = TRUE))) {
        message(
          "For ", regi, " in ", year, ", reference year ", data[regi, year, "Reference_Year"][1],
          " is above ", max(getYears(ghg, as.integer = TRUE)), ". Skipping for now."
        )
      } else {
        # NOTE: the inaccuracy for calculation of GHG factor for CO2/GDP is tolerated
        # target * GDP in target year / GDP in reference year * GHG in reference year
        ghgTarget <- (1 + data[regi, year, conditional]) *
          gdp[regi, year, ] /
          setYears(gdp[regi, round(as.numeric(data[regi, year, "Reference_Year"]) / 5) * 5, ], NULL) *
          setYears(ghg[regi, as.numeric(data[regi, year, "Reference_Year"]), ], NULL)
      }

    } else if (allowedType[data[regi, year, "Type"]] == "GHG-fixed-total") {

      # use the value in the Conditional / Unconditional Column
      ghgTarget <- data[regi, year, conditional]

    } else {
      stop("Unknown Type for regi ", regi, " and year ", year, ": ", data[regi, year, "Type"], " / ",
           allowedType[data[regi, year, "Type"]], " (note: GHG/CAP currently not implemented)")
    }

    return(ghgTarget)
  }

  # countries with known target/2005 ratios bigger than 2.5 or less than 0
  # TODO: adjust once we switch to 2015
  knownHigh <- list("IND" = c(2030))
  knownLow <- list("GAB" = c(2050))

  conditional <- ifelse(length(grep("uncond", subtype)) == 0, "Conditional", "Unconditional")

  ghgFactor <- new.magpie(
    cells_and_regions = getItems(reductionData, dim = 1),
    years = getYears(reductionData),
    names = getNames(gdp),
    sets = c("iso3c", "year", "scenario"),
    fill = NA
  )

  # for each country and year, calculate calculate GHG factor
  for (regi in getItems(reductionData, dim = 1)) {
    for (year in getYears(reductionData, as.integer = TRUE)) {
      if (!is.na(reductionData[regi, year, conditional][1])) {

        y <- if (year < 2060) ceiling((year - 1) / 5) * 5 else ceiling((year - 2) / 10) * 10

        if (regi %in% EUR_NDC_countries && allowedType[reductionData[regi, y, "Type"]] == "GHG-fixed-total") {
          ghg2015 <- sum(setYears(ghg[EUR_NDC_countries, 2015, ], NULL))
        } else {
          ghg2015 <- setYears(ghg[regi, 2015, ], NULL)
        }

        ghgFactor[regi, y, ] <- .calcGhgTarget(reductionData[regi, year, ]) / ghg2015

        ghgFactorMax <- max(c(0, as.numeric(ghgFactor[regi, y, ])), na.rm = TRUE)

        if (isTRUE(ghgFactorMax > 2.5) && !year %in% knownHigh[[regi]] && !regi %in% c("IND", "CHN")) {
          ghgFactor[regi, y, ] <- NA
          message("For ", regi, " in ", year, ", ghgFactor=", ghgFactorMax, " is above 2.5 and will be dropped.")
        }

        ghgFactorMin <- min(c(0, as.numeric(ghgFactor[regi, y, ])), na.rm = TRUE)

        if (isTRUE(ghgFactorMin < 0) && !year %in% knownLow[[regi]]) {
          stop(
            "For ", regi, " in ", year, ", ghgFactor=", ghgFactorMin, " is below 0. ",
            "Is the country really promising negative net emissions? Add it to 'knownLow'."
          )
        }
      }
    }
  }

  return(ghgFactor)
}
