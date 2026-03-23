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



  # 1. Define function to calculate country-level emissions targets depending on target type ----

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

    if ("LULUCF" %in% getNames(data) &&
        data[regi, year, "LULUCF"] > 0 &&
        # actually of target year
        emiRef[regi, 2015, "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"] > 0) {
      ghg <- emiRef[, , "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"]
    } else {
      ghg <- emiRef[, , "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"]
    }

    ghgTarget <- NA

    # 1.Type-GHG-Absolute: An absolute reduction value such as "country will reduce its emissions by 9 Mt"
    if (allowedType[data[regi, year, "Type"]] == "GHG-Absolute") { # absolute GHG change

      if (data[regi, year, "Reference_Year"] == -1) { # -1 if BAU;
        if (!is.na(data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"])) {
          # target + BAU emissions in sheet
          ghgTarget <- data[regi, year, conditional] +
            data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"]
        } else {
          message("For ", regi, " in ", year, ", reference year is BAU, but BAU Emissions are missing.")
          return(ghgTarget)
        }
      } else { # then Reference_Year contains a year
        # target + historic GHG emissions from CEDS (best fit)

        histYear <- min(
          data[regi, year, "Reference_Year"],
          max(getYears(ghg[, c(2030, 2035), , invert = TRUE], as.integer = TRUE))
        )

        if (data[regi, year, "Reference_Year"] > max(getYears(ghg, as.integer = TRUE))) {
          message(
            "For ", regi, " in ", year, ", reference year ", data[regi, year, "Reference_Year"][1],
            " is above ", max(getYears(ghg, as.integer = TRUE)), ", so we use the latter as reference year."
          )
        }

        ghgTarget <- data[regi, year, conditional] +
          setYears(ghg[regi, histYear, ], NULL)
      }
      # 2. Type-GHG-relative: relative reduction of emissions. E.g., "9% reduction compared to base year or BAU"
    } else if (allowedType[data[regi, year, "Type"]] == "GHG") { # relative GHG change

      if (data[regi, year, "Reference_Year"] == -1) { # -1 if BAU.
        if (!is.na(data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"])) {
          # target * BAU emissions in sheet
          ghgTarget <- (1 + data[regi, year, conditional]) *
            data[regi, year, "BAU_or_Reference_emissions_in_MtCO2e"]
        } else {
          message("For ", regi, " in ", year, ", reference year is BAU, but BAU Emissions are missing.")
          return(ghgTarget)
        }
      } else { # then Reference_Year contains a year

        # target * historic GHG emissions from CEDS (best fit)

        histYear <- min(
          data[regi, year, "Reference_Year"],
          max(getYears(ghg[, c(2030, 2035), , invert = TRUE], as.integer = TRUE))
        )

        if (data[regi, year, "Reference_Year"] > max(getYears(ghg, as.integer = TRUE))) {
          message(
            "For ", regi, " in ", year, ", reference year ", data[regi, year, "Reference_Year"][1],
            " is above ", max(getYears(ghg, as.integer = TRUE)), ", so we use the latter as reference year."
          )
        }

        ghgTarget <- (1 + data[regi, year, conditional]) *
          setYears(ghg[regi, histYear, ], NULL)
      }
      # 3. Type CO2 or GHG /GDP: relative reduction of CO2 or GHG intensity
    } else if (allowedType[data[regi, year, "Type"]] %in% c("GHG/GDP", "CO2/GDP")) { # GHG/GDP or CO2/GDP

      if (data[regi, year, "Reference_Year"] > max(getYears(ghg, as.integer = TRUE))) {
        message(
          "For ", regi, " in ", year, ", reference year ", data[regi, year, "Reference_Year"][1],
          " is above ", max(getYears(ghg, as.integer = TRUE)), ". Skipping for now."
        )
      } else {
        # Calculate emissions target from GHG/GDP intensity targets
        # Note that we always use total GHG emissions even if countries might refer to total CO2 emissions only.
        # This small inaccuracy can be tolerated.
        ghgTarget <- (1 + data[regi, year, conditional]) *
          gdp[regi, year, ] /
          setYears(gdp[regi, round(as.numeric(data[regi, year, "Reference_Year"]) / 5) * 5, ], NULL) *
          setYears(ghg[regi, as.numeric(data[regi, year, "Reference_Year"]), ], NULL)
      }

      # 4. Type-GHG-fixed-total: An absolute goal such as "country emissions will be 900 Mt in 2050" or net zero targets
    } else if (allowedType[data[regi, year, "Type"]] == "GHG-fixed-total") {
      # use the value in the Conditional / Unconditional Column
      ghgTarget <- data[regi, year, conditional]
    } else {
      stop(
        "Unknown Type for regi ", regi, " and year ", year, ": ", data[regi, year, "Type"], " / ",
        allowedType[data[regi, year, "Type"]], " (note: GHG/CAP currently not implemented)"
      )
    }


    if ("LULUCF" %in% getNames(data) && data[regi, year, "LULUCF"] > 0 &&
        # actually of target year
        emiRef[regi, 2015, "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"] > 0 &&
        # if ghgTarget could not be set due to an invalid target formulation in the source, skip this step
        year %in% c("y2030", "y2035")) {
      # subtract LULUCF from target to consistently apply Emi|GHG|w/o Bunkers|w/o Land-Use Change
      ghgTarget <- ghgTarget[regi, year, ] - EmiLULUCFTargetYear[regi, year, ]
    }


    return(ghgTarget)
  }

  # 2. Read country-level target formulations and data required to transform them to absolute targets ----
  reductionData <- x

  # load UNFCCC emissions data used for targets relating to reference year emissions
  emiRef <- calcOutput("EmiTargetReference", aggregate = FALSE)

  # GDP SSP trajectories, needed for Emi/GDP intensity targets
  gdp <- calcOutput("GDP", scenario = subset, aggregate = FALSE)

  # define target types allowed
  allowedType <- c("GHG-Absolute",
                   "GHG",
                   "GHG/GDP",
                   "CO2/GDP",
                   "GHG-fixed-total",
                   "GHG/CAP")

  # define conditionality of target ("conditional" or "unconditional" NDC)
  conditional <- ifelse(length(grep("uncond", subtype)) == 0,
                        "Conditional",
                        "Unconditional")





  # 3. Deal with Targets for EU countries ----

  # disaggregate (relative) emissions targets defined for EU(27) to country-level
  # get H12 regionmapping
  regionmapping <- toolGetMapping("regionmappingH12.csv", where = "mappingfolder", type = "regional")

  # apply EU emissions reduction goals uniformly to all countries in EUR region (EU28)
  # for which emissions goals are not defined in the input data
  EUR_NDC_countries <- regionmapping %>%
    filter( .data$RegionCode == "EUR") %>%
    pull( .data$CountryCode)

  # only include countries from EUR region that do not already have defined input data
  # That is, if UK emissions target is defined in input data, this is not overwritten
  # if it is not defined, then UK (country code "GBR") gets the same emissions reductions goal as EU
  EUR_NDC_countries <- setdiff(EUR_NDC_countries,getRegions(reductionData))

  reductionData <- add_columns(reductionData,
                               addnm = EUR_NDC_countries,
                               dim = 1,
                               fill = NA)
  reductionData[EUR_NDC_countries, , ] <- reductionData["EUR", , ]
  reductionData <- reductionData["EUR", , , invert = TRUE]

  # take country-level reference emissions in 1990 for EU countries
  reductionData[EUR_NDC_countries, , "BAU_or_Reference_emissions_in_MtCO2e"] <- emiRef[EUR_NDC_countries, 1990, "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"]


  # 4. Assumptions about LULUCF emissions in target year ----

  # base LULUCF emissions of 2030 and 2035 NDC target years on data from IIASA scenarios
  # this LULUCF data is based on the national accounting logic and is therefore the most consistent available
  # relative to reported UNFCCC emissions for historical periods

  # get LULUCF conditional NDC scenario data from IISAA for 2030
  IIASA_LULUCF_2030 <- readSource("IIASALanduse", subtype = "forecast2030")

  # get LULUCF conditional NDC scenario data from IISAA for 2035
  IIASA_LULUCF_2035 <- readSource("IIASALanduse", subtype = "forecast2035")

  IIASA_LULUCF <- mbind(IIASA_LULUCF_2030,
                        IIASA_LULUCF_2035)

  EmiLULUCFTargetYear <- new.magpie(cells_and_regions = getRegions(reductionData),
                                    years = getYears(reductionData),
                                    names = c("Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"),
                                    fill = 0)

  # LULUCF assumptions for target years 2030 and 2035, for all other target years zero LULUCF contributions assumed
  EmiLULUCFTargetYear <- IIASA_LULUCF[intersect(getRegions(EmiLULUCFTargetYear),getRegions(IIASA_LULUCF)),,]






  # 5. Define plausible range of NDC emissions targets ----

  # define range in which NDC emissions need to be relative to historical 2015 emissions
  # This serves to sort out very unambitious or extremely ambitious targets of single countries
  # that might distort the aggregated target on REMIND region level that is calculated later on
  RelTargetRange <- new.magpie(cells_and_regions = getItems(reductionData, dim = 1),
                               years = paste0("y",seq(2025,2100,5)),
                               names = c("maximum","minimum"),
                               fill = NA)



  RelTargetRange[, "y2025", "maximum"] <- 1.5 # max. 150% of 2015 historical emissions for 2030 target
  RelTargetRange[, "y2025", "minimum"] <- 0.5 # min. 50% of 2015 historical emissions for 2030 target
  RelTargetRange[, "y2030", "maximum"] <- 1.5 # max. 150% of 2015 historical emissions for 2030 target
  RelTargetRange[, "y2030", "minimum"] <- 0.3 # min. 30% of 2015 historical emissions for 2030 target
  RelTargetRange[, "y2035", "maximum"] <- 1.3 # max. 130% of 2015 historical emissions for 2035 target
  RelTargetRange[, "y2035", "minimum"] <- 0 # min. 0 for 2035 target (no net negative emissions targets)

  # no maximum target for India and China because those countries are already REMIND regions so distortion of aggregation is not an issue
  # and they tend to have quite unambitious targets
  RelTargetRange[ c("CHN", "IND"), ,"maximum"] <- NA


  # 6. Calculate country-level absolute emissions targets ----


  # initialize magclass object to store country-level NDC emissions targets
  AbsTarget <- new.magpie(
    cells_and_regions = getItems(reductionData, dim = 1),
    years = getYears(reductionData),
    names = getNames(gdp),
    sets = c("iso3c", "year", "scenario"),
    fill = NA
  )

  # for each country and year, calculate calculate absolute NDC emissions target in MtCO2eq/yr for total GHG emissions excl. bunkers and excl. LULUCF
  for (regi in getItems(reductionData, dim = 1)) {
    for (year in getYears(reductionData, as.integer = TRUE)) {
      if (!is.na(reductionData[regi, year, conditional][1])) {
        # determine target year to define NDC emissions goal for, round to nearest REMIND time step
        y <- if (year < 2060) ceiling((year - 1) / 5) * 5 else ceiling((year - 2) / 10) * 10
        # calculate absolute NDC emissions targets per country
        AbsTarget[regi, y, ] <- .calcGhgTarget(reductionData[regi, year, ])

        # calculate NDC target relative to 2015 historical emissions to perform some plausibility checks
        ghg2015 <- setYears(emiRef[regi, 2015, "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"], NULL)
        TargetRelTo2015 <- AbsTarget[regi, y, ] / collapseNames(ghg2015)



        # check whether targets in plausible range defined by RelTargetRange
        for (i in getNames(TargetRelTo2015)) { # loop over SSPs
          if (!is.na(TargetRelTo2015[,,i]) ) { # check whether target range defined
            # check for too unambitious targets
            if( !is.na(RelTargetRange[regi, y ,"maximum"])) {
              if( TargetRelTo2015[, ,i] > RelTargetRange[regi, y ,"maximum"] ) {
                AbsTarget[regi, y, i] <- NA
                message("For ", regi, " in ", year, " in ", i,
                        ", NDC emissions target level is very unambitious as it is higher than ", as.vector(RelTargetRange[regi, y ,"maximum"]),
                        " times the 2015 emissions and is therefore dropped")
              }
            }

            # check for too ambitious targets
            if( !is.na(RelTargetRange[regi, y ,"minimum"]) ) {
              if( TargetRelTo2015[,,i] < RelTargetRange[regi, y ,"minimum"] ) {
                AbsTarget[regi, y, i] <- NA
                message("For ", regi, " in ", year,
                        ", NDC emissions target level is ambitious as it is lower than ", as.vector(RelTargetRange[regi, y ,"minimum"]),
                        "times the 2015 emissions and is therefore dropped")
              }
            }
          }
        }



      }
    }
  }

  return(AbsTarget)
}
