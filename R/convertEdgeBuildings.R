#' Convert EDGE Buildings data to data on ISO country level.
#'
#' @param subtype either FE or Floorspace
#' @param subset A string (or vector of strings) designating the scenario(s) to be returned.
#' @param x MAgPIE object containing EDGE values at ISO country resolution
#' @return EDGE data as MAgPIE object aggregated to country level
#' @author Antoine Levesque, Robin Hasse
#'
convertEdgeBuildings <- function(x, subtype, subset) {

  .calcLambda <- function(exceedingYearsVec, threshold, previousYears = NULL) {
    exceedingYearsBefore <- exceedingYearsVec[exceedingYearsVec <= threshold]
    exceedingYearsAfter  <- exceedingYearsVec[exceedingYearsVec > threshold]
    lambda <- c(rep(0, length(previousYears)),
                utils::tail(seq(0, 1, length.out = length(exceedingYearsBefore) + 1), -1),
                rep(1, length(exceedingYearsAfter)))
    names(lambda) <- as.character(c(previousYears, exceedingYearsVec))
    return(as.magpie(lambda))
  }

  #---- Parameters and Mappings ------

  rem_years_hist <- seq(1990, 2150, 5)

  # Create data for any missing scenarios (i.e. not in x) by duplication of the SSP2 data.
  xAdd <- purrr::map(subset[!subset %in% getNames(x, dim = "scenario")], function(scen) {
    message(glue::glue("Adding {scen} stationary data as copy of SSP2."))
    setItems(x[, , "SSP2"], 3.1, scen)
  }) %>%
    mbind()
  # Select scenarios
  x <- mbind(x, xAdd) %>% mselect(scenario = subset)

  if (subtype == "FE") {
    #---- Explanations
    # For the historical data, weights are directly taken from the IEA
    # to ensure the consistency at the country level
    # for the future evolution, weights depend on last final energy data point available
    # multiplied by the growth rate of the country

    # Load the regional mapping which depends upon the model used

    mappingfile <- toolGetMapping(type = "regional",
                                  name = "regionmappingEDGE.csv",
                                  returnPathOnly = TRUE,
                                  where = "mappingfolder")
    mapping <- utils::read.csv2(mappingfile)
    region_col <- which(names(mapping) == "RegionCodeEUR_ETP")
    iso_col <- which(names(mapping) == "CountryCode")

    #--- Load the Weights
    ## First load the GDP data for scenarios for which there exist GDP data.
    ## Set average2020 to False to get yearly data as far as possible.
    gdpScen <- subset[subset %in% mrdrivers::toolGetScenarioDefinition(driver = "GDP", aslist = TRUE)$scenario]
    wg <- calcOutput("GDP", scenario = unique(c("SSP2", gdpScen)), average2020 = FALSE, aggregate = FALSE)
    ## For scenarios for which no specific GDP data exists, use SSP2 data.
    wgAdd <- purrr::map(subset[!subset %in% gdpScen], ~setItems(wg[, , "SSP2"], 3, .x)) %>% mbind()
    wg <- mbind(wg, wgAdd) %>% mselect(variable = subset)

    #--- Load final energy data
    wfe <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE)

    #---- Process Data -----------------
    # Replace NAs
    x[is.na(x)] <- 0

    # Select last year of X available in the historical data set
    maxYear_X_in_FE <- max(getYears(x, as.integer = TRUE)[getYears(x, as.integer = TRUE) %in%
                                                            getYears(wfe, as.integer = TRUE)])
    # Deduce the scenario periods
    exceeding_years <- getYears(x, as.integer = TRUE)[getYears(x, as.integer = TRUE) > maxYear_X_in_FE]

    # Scale GDP and FE weights so that they can be added
    wg <- wg / dimSums(wg, dim = 1, na.rm = TRUE)
    wfe <- wfe / dimSums(wfe, dim = 1, na.rm = TRUE)

    # Add the scenario dimension
    wfe <- purrr::map(getNames(x, dim = "scenario"), ~setNames(wfe, paste(.x, getNames(wfe), sep = "."))) %>%
      mbind()

    # Compute lambda
    lambda <- .calcLambda(exceeding_years, 2060)

    # For the future periods, the weight will be a linear combination of last FE weight and of the GDP size.
    # until maxYear_X_in_FE this will be exclusively FE,
    # in 2060 (depending on the threshold value above), exclusively GDP

    wfe <- mbind(wfe,
                 lambda[, exceeding_years, ] * wg[, exceeding_years, ] +
                   (1 - lambda[, exceeding_years, ]) * (setYears(wfe[, maxYear_X_in_FE, ], NULL))
    )

    # Transform weights obtained from calcIOEdgeBuildings to weights compatible with EDGE buildings

    structureMapping <- toolGetMapping(name = "mappingWeightConvertEDGE.csv",
                                       type = "sectoral", where = "mrremind")

    if (!all(unique(structureMapping$EDGE_buildings_items) %in% getNames(x, dim = "item"))) {
      stop("mappingWeightConvertEDGE is missing EDGE buildings items")
    }

    # extend mapping for useful energy
    structureMapping <- structureMapping %>%
      mutate("EDGE_buildings_items" = gsub("_fe$", "_ue", .data$EDGE_buildings_items)) %>%
      rbind(structureMapping)

    edgebWeights  <- do.call("mbind", lapply(unique(structureMapping$EDGE_buildings_items), function(itemName) {
      weightName <- structureMapping[structureMapping$EDGE_buildings_items == itemName, "io_buildings"]
      m <- wfe[, , weightName]
      setNames(m, gsub(weightName, itemName, getNames(m)))
    }))

    wfe <- edgebWeights

    # Check if any of the FE weights are negative
    if (any(wfe < 0)) {
      warning("FE weights computed by calcOutput('IOEdgeBuildings', subtype = X), ",
              "with X in (output_EDGE, output_EDGE_buildings), contain negative values, set to 0")
      wfe[wfe < 0] <- 0
    }

    # Disaggregate and fill the gaps
    weightSum <- toolAggregate(wfe, mappingfile, from = region_col, to = iso_col, dim = 1)

    # only throw the zeroWeight warning in toolAggregate, when any weights are zero,
    # but the corresponding data in x is not 0, as only in these cases the total sum of
    # the magpie object is actually changed

    # see issue: https://github.com/pik-piam/mrremind/issues/678
    if (any(weightSum[x != 0] == 0)) {
      warning(glue::glue(
        "Weight sum is 0, so cannot normalize and will return 0 for some aggregation targets. \\
        This changes the total sum of the magpie object! \\
        Some FE weights are zero for which the corresponding data in x is not zero. \\
        Please check the alignment of historic edgebuildings FE data with the output of \\
        calcOutput('IOEdgeBuildings').")
      )
    }

    # never warn for zero weights, as this is covered by a custom warning
    xadd <- toolAggregate(x, mappingfile, weight = wfe, from = region_col, to = iso_col,
                          zeroWeight = "allow")

    result <- toolCountryFill(xadd, 0, verbosity = 2)

    # Attribute the growth in water heating demand of the EDGE Region OCD to TUR,
    # and retrieve it from AUS, CAN, CHE (Swiss), NOR, NZL
    # For SSP1, SSP2 and SDP
    names_2_change <- grep("(SSP1|SSP2|SDP|SDP_EI|SDP_RC|SDP_MC).*water_heating", getNames(result), value = TRUE)
    names_2_change_elec <- grep("elec", names_2_change, value = TRUE)

    regs_OCD <- c("AUS", "CAN", "CHE", "NOR", "NZL")
    reg_TUR <- "TUR"
    end_of_history <- 2015
    scenario_time <- getYears(result, TRUE)[getYears(result, TRUE) > end_of_history]

    WH_growth <- result[regs_OCD, scenario_time, names_2_change] -
      dimReduce(result[regs_OCD, end_of_history, names_2_change])
    WH_growth[, , names_2_change_elec] <- WH_growth[, , names_2_change_elec] * 0.5
    WH_growth[WH_growth < 0] <- 0
    WH_growth_agg <- dimSums(WH_growth, dim = 1)

    result[getItems(WH_growth, dim = "region"), getYears(WH_growth), getNames(WH_growth)] <-
      result[getItems(WH_growth, dim = "region"), getYears(WH_growth), getNames(WH_growth)] - WH_growth
    result[reg_TUR, getYears(WH_growth), getNames(WH_growth)] <-
      result[reg_TUR, getYears(WH_growth), getNames(WH_growth)] + WH_growth_agg

  }

  if (subtype == "Floorspace") {
    mappingfile <- toolGetMapping(type = "regional",
                                  name = "regionmappingEDGE.csv",
                                  returnPathOnly = TRUE,
                                  where = "mappingfolder")
    mapping <- utils::read.csv2(mappingfile)
    region_col <- which(names(mapping) == "RegionCodeEUR_ETP")
    iso_col <- which(names(mapping) == "CountryCode")

    popScen <- subset[subset %in% mrdrivers::toolGetScenarioDefinition(driver = "Population", aslist = TRUE)$scenario]
    wp <- calcOutput("Population",
                     scenario = unique(c("SSP2", popScen)),
                     years = rem_years_hist,
                     aggregate = FALSE)
    ## For scenarios for which no specific Population data exists, use SSP2 data.
    wpAdd <- purrr::map(subset[!subset %in% popScen], ~setItems(wp[, , "SSP2"], 3, .x)) %>% mbind()
    wp <- mbind(wp, wpAdd) %>% mselect(variable = subset)
    getSets(wp) <- gsub("variable", "scenario", getSets(wp))

    x <- time_interpolate(x, interpolated_year = rem_years_hist, extrapolation_type = "constant")
    x <- toolAggregate(x[, rem_years_hist, ], mappingfile, weight = wp,
                       from = region_col, to = iso_col)
    result <- x
  }

  result
}
