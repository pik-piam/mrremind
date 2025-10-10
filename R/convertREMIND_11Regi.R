#' Converts REMIND regional data
#'
#' @param x MAgPIE object to be converted
#' @param subtype Name of the regional data, e.g. tradecost", "pe2se",
#' "deltacapoffset", "fossilExtractionCoeff", "uraniumExtractionCoeff"
#' @return A MAgPIE object containing country disaggregated data
#' @author original: not defined - tax, fossil and RLDC changes: Renato Rodriguess
#'
convertREMIND_11Regi <- function(x, subtype) {
  if (subtype == "tradecost" | subtype == "storageFactor" | subtype == "ffPolyRent") {
    # No weighting for spatial aggregation
    y <- toolAggregate(x, "regionmappingREMIND.csv", weight = NULL)
  } else if (subtype == "fossilExtractionCoeff") {
    # weight
    oil <- readSource("BGR", subtype = "oil")[, , "Remaining_Potential"]
    gas <- readSource("BGR", subtype = "gas")[, , "Remaining_Potential"]
    coal <- readSource("BGR", subtype = "coal")[, , "Remaining_Potential"]
    BGRData <- list(lowOil = oil, medOil = oil, highOil = oil, lowGas = gas, medGas = gas, highGas = gas, lowCoal = coal, medCoal = coal, highCoal = coal)
    # filling all countries weights and adding a very small weight to countries with no remaining potential to estimate a very step cost curve in this case
    weight <- lapply(BGRData, function(df) {
      df <- toolCountryFill(df, fill = 0, verbosity = 2)
      df[df == 0] <- 1E-20
      return(df)
    })
    #  mapping - original REMIND region mapping (11 regions)
    mapping <- toolGetMapping(type = "regional", name = "regionmappingREMIND.csv", where = "mappingfolder")
    # maxExtraction (upper x limit for function estimation)
    upperBoundMaxExtractionPerCountry <- readSource("REMIND_11Regi", subtype = "ffPolyCumEx")[, , "max"]
    upperBoundMaxExtraction <- toolAggregate(upperBoundMaxExtractionPerCountry, mapping, weight = NULL)
    # return magpie list of country disaggregated coefficients
    countryCoeff <- suppressWarnings(toolCubicFunctionDisaggregate(x, weight, rel = mapping, xUpperBound = upperBoundMaxExtraction))
    # add dimension with the name of the pe on the magpie objects
    output <- lapply(seq_along(countryCoeff), function(i) {
      out <- add_dimension(countryCoeff[[i]], dim = 3.1, nm = names(countryCoeff)[i])
    })
    names(output) <- names(countryCoeff)
    # merge all magpie objects into a single one
    y <- mbind(output)
  } else if (subtype == "uraniumExtractionCoeff") {
    # changes hard coded in REMIND (Summarized from p31_costExPoly modification steps in file \modules\31_fossil\grades2poly\datainput.gms, Rev 7683)
    x[, , "peur.xi1"] <- 25 / 1000
    x[, , "peur.xi2"] <- 0
    x[, , "peur.xi3"] <- ((300 / 1000) * 3**1.8) / ((x[, , "peur.xi3"] * 14 / 4.154) * 3)**2
    x[, , "peur.xi4"] <- 0
    # weight
    BGRuranium <- readSource("BGR", subtype = "uranium")[, , "Remaining_Potential"] # Remaining extraction potential per country
    BGRuranium <- toolCountryFill(BGRuranium, fill = 0, verbosity = 2)
    BGRuranium[BGRuranium == 0] <- 1E-3 # assigning small uranium extraction potential for countries with none to help the curves estimation
    weight <- list(peur = BGRuranium)
    #  mapping - original REMIND region mapping (11 regions)
    mapping <- toolGetMapping(type = "regional", name = "regionmappingREMIND.csv", where = "mappingfolder")
    # maxExtraction (upper x limit for function estimation)
    weightmaxExtraction <- toolAggregate(BGRuranium, mapping, weight = NULL)
    totalWeightMaxExtraction <- as.numeric(colSums(weightmaxExtraction))
    upperBoundMaxExtraction <- 23 * (weightmaxExtraction / totalWeightMaxExtraction) # 23 -> from s31_max_disp_peur -> max global "maximum amount of cumulative uranium production in Megatonnes of metal uranium (U3O8, the stuff that is traded at 40-60US$/lb)."
    upperBoundMaxExtraction <- add_dimension(upperBoundMaxExtraction, dim = 3.1, add = "pe", nm = "peur")
    upperBoundMaxExtraction <- upperBoundMaxExtraction * 1.5 # adding a 50% more to the maximum extraction because the countries shares do not take into consideration the different marginal costs of each country
    # return magpie list of country disaggregated coefficients
    countryCoeff <- suppressWarnings(toolCubicFunctionDisaggregate(x, weight, rel = mapping, xUpperBound = upperBoundMaxExtraction))
    y <- add_dimension(countryCoeff$peur, dim = 3.1, nm = "peur")
  } else if (subtype == "ffPolyCumEx") {
    # use remaining resources available per country as weight
    oil <- readSource("BGR", subtype = "oil")[, , "Remaining_Potential"]
    getNames(oil) <- "peoil"
    gas <- readSource("BGR", subtype = "gas")[, , "Remaining_Potential"]
    getNames(gas) <- "pegas"
    coal <- readSource("BGR", subtype = "coal")[, , "Remaining_Potential"]
    getNames(coal) <- "pecoal"
    # put the data for all fossils together
    bgr <- mbind(oil, coal, gas)
    bgr[is.na(bgr)] <- 0
    # make new magpie object with total dimensions
    w <- new.magpie(getRegions(bgr), getYears(x), getNames(x))
    for (s in getNames(x, dim = 1)) {
      w[, , s] <- bgr[, , s]
    }
    y <- toolAggregate(x, "regionmappingREMIND.csv", weight = w)
    # SB & NB edit 2019/09/11: Increase SSP5 oil max cumulative extraction in USA and CAZ by 20% based on calibration with the SSP IAM project 2017
    y[c("USA", "CAN", "AUS", "NZL", "HMD", "SPM"), , "peoil.max.highOil"] <- y[c("USA", "CAN", "AUS", "NZL", "HMD", "SPM"), , "peoil.max.highOil"] * (1 + 0.2)

    # SB & NB edit 2019/11/18:
    # Increase SSP5 max cumulative coal extraction for USA and CAZ by 20%
    y[c("USA", "CAN", "AUS", "NZL", "HMD", "SPM"), , "pecoal.max.highCoal"] <-
      y[c("USA", "CAN", "AUS", "NZL", "HMD", "SPM"), , "pecoal.max.highCoal"] * (1 + 0.2)
  } else if (subtype == "ccs") {
    # use total land area as weight
    area <- calcOutput("LanduseInitialisation", aggregate = FALSE)[, 2005, ]
    # sum over 3.dimension
    area <- dimSums(area, dim = 3)
    getYears(area) <- NULL
    y <- toolAggregate(x, "regionmappingREMIND.csv", weight = area)
  }
  return(y)
}
