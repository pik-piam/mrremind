#' Convert EDGE Buildings data to data on ISO country level.
#'
#' @param subtype either FE or Floorspace
#' @param x MAgPIE object containing EDGE values at ISO country resolution
#' @return EDGE data as MAgPIE object aggregated to country level
#' @author Antoine Levesque, Robin Hasse
#'
#' @importFrom magclass new.magpie getItems<- getNames getSets getYears mselect mbind

convertEdgeBuildings <- function(x, subtype = "FE") {
  #---- Functions -------------
  noYearDim <- function(x) setYears(x, NULL)

  addSSPnames <- function(x) {
    do.call("mbind", lapply(c(paste0("SSP", c(1:5, "2EU", "2_lowEn")),
                              paste0("SDP", c("", "_EI", "_RC", "_MC")),
                              paste0("SSP2EU_NAV_", c("act", "tec", "ele", "lce", "all")),
                              paste0("SSP2EU_CAMP_", c("weak", "strong"))),
      function(s) setNames(x, paste(s, getNames(x), sep = "."))
    ))
  }

  duplScens <- function(x, scens = NULL) {
    if (is.null(scens)) {
      scens <- list(
        gdp_SSP2EU = paste0("gdp_SSP2EU_",
                            c("NAV_act", "NAV_ele", "NAV_tec", "NAV_lce", "NAV_all",
                              "CAMP_weak", "CAMP_strong")),
        gdp_SSP2 = "gdp_SSP2_lowEn"
      )
    }
    mbind(x, do.call(mbind, lapply(names(scens), function(from) {
      do.call(mbind, lapply(scens[[from]], function(to) {
        setItems(x[, , from], 3, to)
      }))
    })))
  }

  renameExtraWeights <- function(magObj, magWeight, mapping) {
    do.call("mbind", lapply(mapping[["EDGEitems"]], function(itemIN) {
      if (itemIN %in% getNames(magObj, dim = "item")) {
        item_weight <- mapping[mapping$EDGEitems == itemIN, "weight_convertEDGE"]
        sub_magpie <- magWeight[, , item_weight]
        res <- setNames(sub_magpie, gsub(item_weight, itemIN, getNames(sub_magpie)))
      } else {
        res <- NULL
      }
      return(res)
    }))
  }

  calcLambda <- function(exceeding_years_vec, threshold, previous_years = NULL) {
    exceeding_years_before <- exceeding_years_vec[exceeding_years_vec <= threshold]
    exceeding_years_after  <- exceeding_years_vec[exceeding_years_vec > threshold]
    lambda <- c(rep(0, length(previous_years)),
                tail(seq(0, 1, length.out = length(exceeding_years_before) + 1), -1),
                rep(1, length(exceeding_years_after)))
    names(lambda) <- as.character(c(previous_years, exceeding_years_vec))
    return(as.magpie(lambda))
  }


  #---- Parameters and Mappings ------
  rem_years_hist <- seq(1990, 2150, 5)

  struct_mapping_path <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv",
                                      returnPathOnly = TRUE, where = "mrcommons")
  struct_mapping <- read.csv2(struct_mapping_path, na.strings = "")

  # Select the relevant part of the mapping
  struct_mapping <- struct_mapping[!is.na(struct_mapping$weight_convertEDGE), ]
  struct_mapping <- unique(struct_mapping[c("weight_convertEDGE", "EDGEitems")])


  if (subtype == "FE") {
    #---- Explanations
    # For the historical data, weights are directly taken from the IEA
    # to ensure the consistency at the country level
    # for the future evolution, weights depend on last final energy data point available
    # multiplied by the growth rate of the country

    # Load the regional mapping which depends upon the model used

    mappingfile <- toolGetMapping(type = "regional", name = "regionmappingEDGE.csv",
                                  returnPathOnly = TRUE, where = "mappingfolder")
    mapping <- read.csv2(mappingfile)
    region_col <- which(names(mapping) == "RegionCodeEUR_ETP")
    iso_col <- which(names(mapping) == "CountryCode")

    #--- Load the Weights
    #--- First load the GDP data. Set average2020 to False to get yearly data as far as possible.
    wg <- calcOutput("GDP", average2020 = FALSE, aggregate = FALSE)
    # duplicate SSP2 for SSP2_lowEn an SSP2EU for Navigate and Campaigners scenarios
    wg <- duplScens(wg)
    getNames(wg) <- gsub("gdp_", "", getNames(wg))

    #--- Then load the final energy data
    hist_fe_stationary <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE", aggregate = FALSE)
    hist_fe_buildings <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE)

    wfe <- mbind(hist_fe_stationary, hist_fe_buildings)

    #---- Process Data -----------------

    # Replace NAs
    x[is.na(x)] <- 0

    if (any(wfe < 0)) {
      warning("calcOutput('IOEdgeBuildings', subtype = X), with X in (output_EDGE, output_EDGE_buildings) produces negative values, set to 0")
      wfe[wfe < 0] <- 0
    }

    # Select last year of X available in the historical data set
    maxYear_X_in_FE <- max(getYears(x, as.integer = TRUE)[getYears(x, as.integer = TRUE) %in% getYears(wfe, as.integer = TRUE)])
    # Deduce the scenario periods
    exceeding_years <- getYears(x, as.integer = TRUE)[getYears(x, as.integer = TRUE) > maxYear_X_in_FE]

    # Scale GDP and FE weights so that they can be added
    wg <- wg / dimSums(wg, dim = 1, na.rm = TRUE)
    wfe <- wfe / dimSums(wfe, dim = 1, na.rm = TRUE)

    # Add some corrections
    wg[is.na(wg)] <- 0
    wg[wg == "Inf"] <- 0

    # Add some corrections to the FE data set + add the scenario dimension
    wfe[is.na(wfe)] <- 0
    wfe <- addSSPnames(wfe)

    # Compute lambda
    lambda <- calcLambda(exceeding_years, 2060)
    # For the future periods, the weight will be a linear combination of last FE weight and of the GDP size.
    # until maxYear_X_in_FE this will be exclusively FE, in 2060 (depending on the threshold value above), exclusively GDP

    wfe <- mbind(wfe,
      lambda[, exceeding_years, ] * wg[, exceeding_years, ] +
        (1 - lambda[, exceeding_years, ]) * (noYearDim(wfe[, maxYear_X_in_FE, ]))
    )

    # In cases where the variables in EDGE do not exist in the mapping for computing the final energy,
    # e.g. when EDGE produces further disaggregations, or when it gives REMIND items without computing them
    wfe <- mbind(wfe, renameExtraWeights(x, wfe, struct_mapping))

    # Reduce the dimensions of the weights
    wfe <- wfe[, getYears(x), getNames(x, dim = "item")]

    # Disaggregate and fill the gaps
    xadd <- toolAggregate(x, mappingfile, weight = wfe,
                          from = region_col,
                          to = iso_col)
    result <- toolCountryFill(xadd, 0)

    # Attribute the growth in water heating demand of the EDGE Region OCD to TUR,
    # and retrieve it from AUS, CAN, CHE (Swiss), NOR, NZL
    # For SSP1, SSP2 and SDP
    names_2_change <- grep("(SSP1|SSP2|SDP|SDP_EI|SDP_RC|SDP_MC|SSP2EU).*water_heating", getNames(result), value = TRUE)
    names_2_change_elec <- grep("elec", names_2_change, value = TRUE)

    regs_OCD <- c("AUS", "CAN", "CHE", "NOR", "NZL")
    reg_TUR <- "TUR"
    end_of_history <- 2015
    scenario_time <- getYears(result, TRUE)[getYears(result, TRUE) > end_of_history]

    WH_growth <- result[regs_OCD, scenario_time, names_2_change] -  dimReduce(result[regs_OCD, end_of_history, names_2_change])
    WH_growth[, , names_2_change_elec] <- WH_growth[, , names_2_change_elec] * 0.5
    WH_growth[WH_growth < 0] <- 0
    WH_growth_agg <- dimSums(WH_growth, dim = 1)

    result[getRegions(WH_growth), getYears(WH_growth), getNames(WH_growth)] <- result[getRegions(WH_growth), getYears(WH_growth), getNames(WH_growth)] - WH_growth
    result[reg_TUR, getYears(WH_growth), getNames(WH_growth)] <- result[reg_TUR, getYears(WH_growth), getNames(WH_growth)] + WH_growth_agg

  } else if (subtype == "Floorspace") {
    mappingfile <- toolGetMapping(type = "regional", name = "regionmappingEDGE.csv",
                                  returnPathOnly = TRUE, where = "mappingfolder")
    mapping <- read.csv2(mappingfile)
    region_col <- which(names(mapping) == "RegionCodeEUR_ETP")
    iso_col <- which(names(mapping) == "CountryCode")

    getNames(x) <- paste0("gdp_", getNames(x))
    wp <- calcOutput("Population", years = rem_years_hist, aggregate = FALSE)
    getSets(wp) <- gsub("variable", "scenario", getSets(wp))
    getItems(wp, "scenario") <- gsub("pop_", "gdp_", getItems(wp, "scenario"))
    # duplicate SSP2 for SSP2_lowEn an SSP2EU for Navigate and Campaigners scenarios
    wp <- duplScens(wp)

    x <- toolAggregate(x[, rem_years_hist, ], mappingfile, weight = wp,
                       from = region_col, to = iso_col)
    result <- x
  }
  return(result)
}
