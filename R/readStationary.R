#' Load Stationary File as magclass object
#'
#' Reads in EDGE_TradMod.cs4r, which hast data for the SSP scenarios. In the convert function, the subset argument
#' is used to specify the desired scenarios. Unspecified scenarios are copies of the SSP2 scenario.
#'
#' @return magclass object
#' @order 1
#' @author Antoine Levesque, Robin Hasse
#'
readStationary <- function() {
  read.magpie("EDGE_TradMod.cs4r")
}

#' @rdname readStationary
#' @order 2
#' @param x MAgPIE object returned from readStationary
#' @param subset A string, or vector of string designating the entries for the scenario dimension. If possible,
#'  passed to [mrdrivers::calcGDP()]. Unspecified scenarios receive SSP2 data.
convertStationary <- function(x, subset) {
  #---- Parameters and Mappings ------
  structMappingPath <- toolGetMapping(type = "sectoral",
                                      name = "structuremappingIO_outputs.csv",
                                      returnPathOnly = TRUE,
                                      where = "mrcommons")
  structMapping <- utils::read.csv2(structMappingPath, na.strings = "")
  # Select the relevant part of the mapping
  structMapping <- structMapping[!is.na(structMapping$weight_convertEDGE), ]
  structMapping <- unique(structMapping[c("weight_convertEDGE", "EDGEitems")])

  x[is.na(x)] <- 0
  getSets(x) <- c("region", "year", "scenario", "item")

  # Create data for any missing scenarios (i.e. not in x) by duplication of the SSP2 data.
  xAdd <- purrr::map(subset[!subset %in% getNames(x, dim = "scenario")], function(scen) {
    message(glue::glue("Adding {scen} EdgeBuildings data as copy of SSP2."))
    setItems(x[, , "SSP2"], "scenario", scen)
  }) %>%
    mbind()
  # Select scenarios.
  x <- mbind(x, xAdd) %>% mselect(scenario = subset)

  #---- Explanations
  # For the historical data, weights are directly taken from the IEA
  # to ensure the consistency at the country level
  # for the future evolution, weights depend on last final energy data point available
  # multiplied by the growth rate of the country

  # Load the regional mapping which depends upon the model used
  mappingfile <- toolGetMapping(type = "regional",
                                name = "regionmappingREMIND.csv",
                                returnPathOnly = TRUE,
                                where = "mappingfolder")
  mapping <- utils::read.csv2(mappingfile)
  regionCol <- which(names(mapping) == "RegionCode")
  isoCol <- which(names(mapping) == "CountryCode")

  #--- Load the Weights
  ## First load the GDP data for scenarios for which there exist GDP data.
  ## Set average2020 to False to get yearly data as far as possible.
  gdpScen <- subset[subset %in% mrdrivers::toolGetScenarioDefinition(driver = "GDP", aslist = TRUE)$scenario]
  wg <- calcOutput("GDP", scenario = unique(c("SSP2", gdpScen)), average2020 = FALSE, aggregate = FALSE)
  ## For scenarios for which no specific GDP data exists, use SSP2 data.
  wgAdd <- purrr::map(subset[!subset %in% gdpScen], ~setItems(wg[, , "SSP2"], 3, .x)) %>% mbind()
  wg <- mbind(wg, wgAdd) %>% mselect(variable = subset)

  ## Then load the final energy data
  histFeStationary <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE", aggregate = FALSE)
  histFeBuildings <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE)
  wfe <- mbind(histFeStationary, histFeBuildings)
  if (any(wfe < 0)) {
    warning("calcOutput('IOEdgeBuildings', subtype = X), with X in (output_EDGE, output_EDGE_buildings) produces negative values, set to 0") # nolint
    wfe[wfe < 0] <- 0
  }


  #---- Process Data -----------------
  # Replace NAs
  x[is.na(x)] <- 0

  # Select last year of X available in the historical data set
  maxYearXInFE <- max(getYears(x, as.integer = TRUE)[getYears(x, as.integer = TRUE) %in%
                                                       getYears(wfe, as.integer = TRUE)])
  # Deduce the scenario periods
  exceedingYears <- getYears(x, as.integer = TRUE)[getYears(x, as.integer = TRUE) > maxYearXInFE]


  # FE_stationary projections are not updated. Therefore, we correct here for the newly published past data
  # For historical years, the data is substituted. For projections years, there is first a transition period,
  # before the FE_stationary projections are fully taken up

  # The years exceeding maxYear might not be meaningful. Therefore we exclude them.
  helper <- getYears(histFeStationary)[getYears(histFeStationary, TRUE) <= maxYearXInFE]
  feStationary <- time_interpolate(histFeStationary[, helper, ],
                                   interpolated_year = c(maxYearXInFE, exceedingYears),
                                   integrate_interpolated_years = TRUE,
                                   extrapolation_type = "constant")
  # Duplicate data for all scenarios.
  feStationary <- purrr::map(getNames(x, dim = "scenario"),
                             ~setNames(feStationary, paste(.x, getNames(feStationary), sep = "."))) %>%
    mbind()

  # change the regional resolution of feStationary to match the EDGE_stationary resolution
  # isoCol and regionCol are originally designed for the weights, that is why names are confusing here
  feStationary <- toolAggregate(feStationary, mappingfile, from = isoCol, to = regionCol)

  # Item names differ slightly for the input of EDGE_stationary (feStationary) and the output
  # The main issue concerns transport. We therefore restrict to the variables of interest in each data set of
  # historical data
  # Stationary, non-buildings names
  stationaryItems <- grep("^(fenon|feagr|feind|feoth)", getNames(x, TRUE)[[2]], value = TRUE)


  # create lambda vector that gives 0 to the historical data and 1 after 2030
  lambda <- toolCalcLambda(exceedingYears, 2030, getYears(x)[getYears(x, TRUE) <= maxYearXInFE])
  # Replace
  x[, , stationaryItems] <- feStationary[, getYears(x), stationaryItems] * (1 - lambda) +
    x[, , stationaryItems] * lambda

  # Scale GDP and FE weights so that they can be added
  wg <- wg / dimSums(wg, dim = 1, na.rm = TRUE)
  wfe <- wfe / dimSums(wfe, dim = 1, na.rm = TRUE)

  # Add the scenario dimension
  wfe <- purrr::map(getNames(x, dim = "scenario"), ~setNames(wfe, paste(.x, getNames(wfe), sep = "."))) %>%
    mbind()

  # Compute lambda
  lambda <- toolCalcLambda(exceedingYears, 2060)
  # For the future periods, the weight will be a linear combination of last FE weight and of the GDP size.
  # until maxYearXInFE this will be exclusively FE, in 2060 (depending on the threshold value above), exclusively GDP
  wfe <- mbind(
    wfe,
    lambda[, exceedingYears, ] * wg[, exceedingYears, ] +
      (1 - lambda[, exceedingYears, ]) * (setYears(wfe[, maxYearXInFE, ], NULL))
  )

  # In cases where the variables in EDGE do not exist in the mapping for computing the final energy,
  # e.g. when EDGE produces further disaggregations, or when it gives REMIND items without computing them
  wfe <- mbind(wfe, toolRenameExtraWeights(x, wfe, structMapping))

  # Reduce the dimensions of the weights
  wfe <- wfe[, getYears(x), getNames(x, dim = "item")]

  # Disaggregate and fill the gaps
  xadd <- toolAggregate(x, mappingfile, weight = wfe, from = regionCol, to = isoCol)
  toolCountryFill(xadd, 0, verbosity = 2)
}

toolRenameExtraWeights <- function(magObj, magWeight, mapping) {
  do.call("mbind", lapply(mapping[["EDGEitems"]], function(itemIN) {
    if (itemIN %in% getNames(magObj, dim = "item")) {
      item_weight <- mapping[mapping$EDGEitems == itemIN, "weight_convertEDGE"]
      subMagpie <- magWeight[, , item_weight]
      res <- setNames(subMagpie, gsub(item_weight, itemIN, getNames(subMagpie)))
    } else {
      res <- NULL
    }
    return(res)
  }))
}

toolCalcLambda <- function(exceedingYearsVec, threshold, previousYears = NULL) {
  exceedingYearsBefore <- exceedingYearsVec[exceedingYearsVec <= threshold]
  exceedingYearsAfter  <- exceedingYearsVec[exceedingYearsVec > threshold]
  lambda <- c(rep(0, length(previousYears)),
              utils::tail(seq(0, 1, length.out = length(exceedingYearsBefore) + 1), -1),
              rep(1, length(exceedingYearsAfter)))
  names(lambda) <- as.character(c(previousYears, exceedingYearsVec))
  return(as.magpie(lambda))
}
