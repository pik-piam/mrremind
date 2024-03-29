#' Convert Stationary data to data on ISO country level.
#'
#' @author Antoine Levesque, Robin Hasse
#' @param x MAgPIE object to be converted
convertStationary <- function(x) {

  #---- Functions -------------
  noYearDim <- function(x) setYears(x, NULL)

  addSSPnames <- function(x) {
    do.call("mbind", lapply(c(paste0("SSP", c(1:5, "2EU", "2_lowEn")),
                              paste0("SDP", c("", "_EI", "_RC", "_MC"))),
                            function(s) setNames(x, paste(s, getNames(x), sep = "."))
    ))
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

  struct_mapping_path <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv",
                                        returnPathOnly = TRUE, where = "mrcommons")
  struct_mapping <- read.csv2(struct_mapping_path, na.strings = "")

  # Select the relevant part of the mapping
  struct_mapping <- struct_mapping[!is.na(struct_mapping$weight_convertEDGE), ]
  struct_mapping <- unique(struct_mapping[c("weight_convertEDGE", "EDGEitems")])


  #---- Explanations
  # For the historical data, weights are directly taken from the IEA
  # to ensure the consistency at the country level
  # for the future evolution, weights depend on last final energy data point available
  # multiplied by the growth rate of the country

  # Load the regional mapping which depends upon the model used

  mappingfile <- toolGetMapping(type = "regional", name = "regionmappingREMIND.csv",
                                returnPathOnly = TRUE, where = "mappingfolder")
  mapping <- read.csv2(mappingfile)
  region_col <- which(names(mapping) == "RegionCode")
  iso_col <- which(names(mapping) == "CountryCode")

  #--- Load the Weights
  #--- First load the GDP data. Set average2020 to False to get yearly data as far as possible.
  wg <- calcOutput("GDP", average2020 = FALSE, aggregate = FALSE)
  getNames(wg) <- gsub("gdp_", "", getNames(wg))
  # duplicate SSP2 for SSP2_lowEn
  wg <- mbind(wg, setItems(wg[, , "SSP2"], 3, "SSP2_lowEn"))


  #--- Then load the final energy data
  hist_fe_stationary <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE", aggregate = FALSE)
  hist_fe_buildings <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE)
  hist_fe_transport <- calcOutput("IO", subtype = "output", aggregate = FALSE)

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


  # FE_stationary projections are not updated. Therefore, we correct here for the newly published past data
  # For historical years, the data is substituted. For projections years, there is first a transition period,
  # before the FE_stationary projections are fully taken up

  fe_stationary <- time_interpolate(hist_fe_stationary[, getYears(hist_fe_stationary)[getYears(hist_fe_stationary, TRUE) <= maxYear_X_in_FE], ], # The years exceeding maxYear might not be meaningful. Therefore we exclude them
                                    interpolated_year = c(maxYear_X_in_FE, exceeding_years),
                                    integrate_interpolated_years = TRUE,
                                    extrapolation_type = "constant")
  fe_stationary <- addSSPnames(fe_stationary)

  fe_transport <- time_interpolate(hist_fe_transport[, getYears(hist_fe_transport)[getYears(hist_fe_transport, TRUE) <= maxYear_X_in_FE], ], # The years exceeding maxYear might not be meaningful. Therefore we exclude them
                                   interpolated_year = c(maxYear_X_in_FE, exceeding_years),
                                   integrate_interpolated_years = TRUE,
                                   extrapolation_type = "constant")
  fe_transport <- addSSPnames(fe_transport)
  getSets(fe_transport) <- c("region", "period", "scenario", "input", "output", "tech")

  # change the regional resolution of fe_stationary to match the EDGE_stationary resolution
  # iso_col and region_col are originally designed for the weights, that is why names are confusing here
  fe_stationary <- toolAggregate(fe_stationary, mappingfile, from = iso_col, to = region_col)
  fe_transport <- toolAggregate(fe_transport, mappingfile, from = iso_col, to = region_col)

  # Item names differ slightly for the input of EDGE_stationary (fe_stationary) and the output
  # The main issue concerns transport. We therefore restrict to the variables of interest in each data set of
  # historical data
  stationary_items <- grep("^(fenon|feagr|feind|feoth)", getNames(x, TRUE)[[2]], value = TRUE) # Stationary, non-buildings names
  transport_items <- grep("^(fepet|fedie|feelt)", getNames(x, TRUE)[[2]], value = TRUE) # Transport names

  # simplify transport
  fe_transport <- dimSums(fe_transport, dim = c("input", "tech"))

  # create lambda vector that gives 0 to the historical data and 1 after 2030
  lambda <-  calcLambda(exceeding_years, 2030, getYears(x)[getYears(x, TRUE) <= maxYear_X_in_FE])
  # Replace
  x[, , stationary_items] <- fe_stationary[, getYears(x), stationary_items] * (1 - lambda) + x[, , stationary_items] * lambda
  x[, , transport_items] <- fe_transport[, getYears(x), transport_items] * (1 - lambda) + x[, , transport_items] * lambda

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
  result <- toolCountryFill(xadd, 0, verbosity = 2)

  # re-calculating fepet and fedie final energy based on updated EDGE shares
  share <- readSource(type = "EDGETransport", subtype = "shares_LDV_transport")
  # for EU regions use JRC data instead
  JRC_reg <- c("MLT", "EST", "CYP", "LVA", "LTU", "LUX", "SVK", "SVN", "HRV", "BGR", "HUN", "ROU", "FIN", "DNK", "IRL", "CZE", "GRC", "AUT", "PRT", "SWE", "BEL", "NLD", "POL", "ESP", "ITA", "GBR", "FRA", "DEU")
  JRC <- calcOutput("JRC_IDEES", subtype = "Transport", aggregate = FALSE)
  JRC_share <- new.magpie(JRC_reg, getYears(share), getNames(share), fill = 0)
  # for years lower or equal to 2015 assume bunkers equal to JRC historical values
  y1 <- getYears(JRC)[getYears(JRC, as.integer = TRUE) <= 2015]
  JRC_share[JRC_reg, y1, ] <- JRC[JRC_reg, y1, "FE|Transport|LDV|Liquids (EJ/yr)"] / (JRC[JRC_reg, y1, "FE|Transport|non-LDV|Liquids (EJ/yr)"] + JRC[JRC_reg, y1, "FE|Transport|LDV|Liquids (EJ/yr)"])
  # for years after 2015 assume LDV share constant and eqaul to JRC 2015 values
  y2 <- getYears(share)[getYears(share, as.integer = TRUE) > 2015]
  JRC_share[, y2, ] <- JRC_share[, 2015, ]
  ## setting EU shares equal to JRC values
  varname_SSP2 <- getNames(share[, , "gdp_SSP2"])[1]
  share[JRC_reg, getYears(JRC_share), varname_SSP2] <- JRC_share[JRC_reg, getYears(JRC_share), ]
  # redefining LDV and non-LDV liquids
  feTotal <- dimSums(result[, , c("fepet", "fedie")], dim = 3.2)
  feShares <- new.magpie(cells_and_regions = getRegions(share), years = intersect(getYears(share), getYears(result)), names = getNames(result[, , c("fepet", "fedie")]))
  feShares[, , "fepet"] <- setNames(setNames(share[getRegions(share), getYears(feShares), "share_LDV_totliq"], "fepet"), NULL)
  feShares[, , "fedie"] <- (1 - setNames(setNames(share[getRegions(share), getYears(feShares), "share_LDV_totliq"], "fepet"), NULL))
  feTransp <- new.magpie(cells_and_regions = getRegions(share), years = getYears(feShares), names = getNames(result[, , c("fepet", "fedie")]))

  for (i in getNames(result, dim = 1)) {
    i1 <- paste0(i, ".fepet")
    i2 <- paste0(i, ".fedie")
    feTransp[, getYears(feShares), i1] <- feShares[, getYears(feShares), i1] * setNames(feTotal[, getYears(feShares), i], i1)
    feTransp[, getYears(feShares), i2] <- feShares[, getYears(feShares), i2] * setNames(feTotal[, getYears(feShares), i], i2)
  }

  # extrapolating missing historical years
  result[, getYears(feTransp), getNames(feTransp)] <- feTransp[, getYears(feTransp), getNames(feTransp)]

  # fix issue with trains in transport trajectories: they seem to be 0 for t > 2100
  if (all(mselect(result, year = "y2105", scenario = "SSP2", item = "feelt") == 0)) {
    result[, seq(2105, 2150, 5), "feelt"] <- time_interpolate(result[, 2100, "feelt"], seq(2105, 2150, 5))
  }

  return(result)
}
