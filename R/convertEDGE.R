#' Convert EDGE
#'
#' Convert EDGE data to data on ISO country level.
#'
#' @param subtype FE for final energy or Capital for capital projections
#' @param x MAgPIE object containing EDGE values at ISO country resolution
#' @return EDGE data as MAgPIE object aggregated to country level
#' @author Antoine Levesque, Robin Hasse
#'
#' @importFrom magclass new.magpie getItems<- getNames getSets getYears mselect
#' mbind

convertEDGE <- function(x, subtype = "FE_stationary") {

  #---- Functions -------------
  noYearDim <- function(x) setYears(x, NULL)

  addSSPnames <- function(x) {
    do.call("mbind", lapply(c(paste0("SSP", c(1:5, "2EU", "2_lowEn")),
                              paste0("SDP", c("", "_EI", "_RC", "_MC")),
                              paste0("SSP2EU_NAV_", c("act", "tec", "ele", "lce", "all"))),
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
  rem_years_hist <- seq(1990,2150,5)
  keep_years <- getYears(x)

  struct_mapping_path = toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", returnPathOnly = TRUE, where = "mappingfolder")
  struct_mapping = read.csv2(struct_mapping_path, na.strings = "")

  #Select the relevant part of the mapping
  struct_mapping = struct_mapping[!is.na(struct_mapping$weight_convertEDGE),]
  struct_mapping = unique(struct_mapping[c( "weight_convertEDGE", "EDGEitems")])


  if (subtype %in% c("FE_stationary","FE_buildings")){
    #---- Explanations
    # For the historical data, weights are directly taken from the IEA
    # to ensure the consistency at the country level
    # for the future evolution, weights depend on last final energy data point available
    # multiplied by the growth rate of the country

    # Load the regional mapping which depends upon the model used
    if (subtype == "FE_stationary"){

      mappingfile <- toolGetMapping(type = "regional", name = "regionmappingREMIND.csv", returnPathOnly = TRUE, where = "mappingfolder")
      mapping <- read.csv2(mappingfile)
      region_col = which(names(mapping) == "RegionCode")
      iso_col = which(names(mapping) == "CountryCode")

    }else if (subtype %in% c("FE_buildings")){

      mappingfile <- toolGetMapping(type = "regional", name = "regionmappingEDGE.csv", returnPathOnly = TRUE, where = "mappingfolder")
      mapping <- read.csv2(mappingfile)
      region_col = which(names(mapping) == "RegionCodeEUR_ETP")
      iso_col = which(names(mapping) == "CountryCode")

    }

    #--- Load the Weights
    #--- First load the GDP data. Set average2020 to False to get yearly data as far as possible.
    wg <- calcOutput("GDP", average2020 = FALSE, aggregate = FALSE)
    # duplicate SSP2 for SSP2_lowEn an SSP2EU for Navigate scenarios
    wg <- mbind(
      wg,
      do.call("mbind", lapply(paste0("gdp_SSP2EU_NAV_", c("act", "tec", "ele", "lce", "all")),
                              function(navScen) {
                                setItems(wg[,, "gdp_SSP2EU"], 3, navScen)
                              })),
      setItems(wg[,, "gdp_SSP2"], 3, "gdp_SSP2_lowEn")
    )
    getNames(wg) <- gsub("gdp_", "", getNames(wg))


    #--- Then load the final energy data
    hist_fe_stationary = calcOutput("IOEdgeBuildings", subtype = "output_EDGE", aggregate = F)
    hist_fe_buildings = calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = F)
    hist_fe_transport = calcOutput("IO", subtype="output", aggregate = F)

    wfe <- mbind(hist_fe_stationary, hist_fe_buildings)


    #---- Process Data -----------------

    # Replace NAs
    x[is.na(x)] <- 0

    if (any(wfe < 0 )){
      warning("calcOutput('IOEdgeBuildings', subtype = X), with X in (output_EDGE, output_EDGE_buildings) produces negative values, set to 0")
      wfe[wfe < 0 ] = 0
    }

    #Select last year of X available in the historical data set
    maxYear_X_in_FE = max(getYears(x, as.integer = T)[getYears(x, as.integer = T) %in% getYears(wfe, as.integer = T)])
    # Deduce the scenario periods
    exceeding_years = getYears(x, as.integer = T)[getYears(x, as.integer = T) > maxYear_X_in_FE]


    #FE_stationary projections are not updated. Therefore, we correct here for the newly published past data
    #For historical years, the data is substituted. For projections years, there is first a transition period,
    #before the FE_stationary projections are fully taken up
    if(subtype == "FE_stationary"){
      fe_stationary = time_interpolate(hist_fe_stationary[,getYears(hist_fe_stationary)[getYears(hist_fe_stationary,T) <= maxYear_X_in_FE],], # The years exceeding maxYear might not be meaningful. Therefore we exclude them
                              interpolated_year = c(maxYear_X_in_FE,exceeding_years),
                              integrate_interpolated_years = T,
                              extrapolation_type = "constant")
      fe_stationary = addSSPnames(fe_stationary)

      fe_transport = time_interpolate(hist_fe_transport[,getYears(hist_fe_transport)[getYears(hist_fe_transport,T) <= maxYear_X_in_FE],], # The years exceeding maxYear might not be meaningful. Therefore we exclude them
                                       interpolated_year = c(maxYear_X_in_FE,exceeding_years),
                                       integrate_interpolated_years = T,
                                       extrapolation_type = "constant")
      fe_transport = addSSPnames(fe_transport)
      getSets(fe_transport) = c("region","period","scenario","input","output","tech")

      #change the regional resolution of fe_stationary to match the EDGE_stationary resolution
      #iso_col and region_col are originally designed for the weights, that is why names are confusing here
      fe_stationary = toolAggregate(fe_stationary,mappingfile, from = iso_col, to = region_col)
      fe_transport = toolAggregate(fe_transport,mappingfile, from = iso_col, to = region_col)

      #Item names differ slightly for the input of EDGE_stationary (fe_stationary) and the output
      #The main issue concerns transport. We therefore restrict to the variables of interest in each data set of
      #historical data
      stationary_items = grep("^(fenon|feagr|feind|feoth)", getNames(x,T)[[2]], value = T) # Stationary, non-buildings names
      transport_items = grep("^(fepet|fedie|feelt)", getNames(x,T)[[2]], value = T) #Transport names

      #simplify transport
      fe_transport = dimSums(fe_transport,dim = c("input","tech"))

      #create lambda vector that gives 0 to the historical data and 1 after 2030
      lambda =  calcLambda(exceeding_years,2030,getYears(x)[getYears(x,T) <= maxYear_X_in_FE ])
      #Replace
      x[,,stationary_items] = fe_stationary[,getYears(x),stationary_items] * (1 - lambda) + x[,,stationary_items] * lambda
      x[,,transport_items] = fe_transport[,getYears(x),transport_items] * (1 - lambda) + x[,,transport_items] * lambda
    }

    # Scale GDP and FE weights so that they can be added
    wg = wg/dimSums(wg, dim = 1, na.rm = T)
    wfe = wfe/dimSums(wfe, dim =1, na.rm = T)

    # Add some corrections
    wg[is.na(wg)] <- 0
    wg[wg == "Inf"] <- 0

    # Add some corrections to the FE data set + add the scenario dimension
    wfe[is.na(wfe)] <- 0
    wfe = addSSPnames(wfe)

    # Compute lambda
    lambda = calcLambda(exceeding_years, 2060)
    # For the future periods, the weight will be a linear combination of last FE weight and of the GDP size.
    # until maxYear_X_in_FE this will be exclusively FE, in 2060 (depending on the threshold value above), exclusively GDP

    wfe <- mbind(wfe,
      lambda[,exceeding_years, ] * wg[,exceeding_years, ] +
        (1 - lambda[, exceeding_years, ]) * (noYearDim(wfe[, maxYear_X_in_FE, ]))
    )

    # In cases where the variables in EDGE do not exist in the mapping for computing the final energy,
    #e.g. when EDGE produces further disaggregations, or when it gives REMIND items without computing them
    wfe = mbind(wfe, renameExtraWeights(x,wfe, struct_mapping))

    #Reduce the dimensions of the weights
    wfe = wfe[,getYears(x), getNames(x, dim = "item")]

    #Disaggregate and fill the gaps
    xadd <- toolAggregate(x,mappingfile,weight=wfe,
                          from = region_col,
                          to = iso_col)
    result <- toolCountryFill(xadd,0)

    if(subtype == "FE_stationary"){
      # re-calculating fepet and fedie final energy based on updated EDGE shares
      share <- readSource(type="EDGETransport", subtype = "shares_LDV_transport")
      # for EU regions use JRC data instead
      JRC_reg <- c("MLT","EST","CYP","LVA","LTU","LUX","SVK","SVN","HRV","BGR","HUN","ROU","FIN","DNK","IRL","CZE","GRC","AUT","PRT","SWE","BEL","NLD","POL","ESP","ITA","GBR","FRA","DEU")
      JRC <- calcOutput("JRC_IDEES", subtype="Transport", aggregate = FALSE)
      JRC_share <- new.magpie(JRC_reg,getYears(share),getNames(share),fill=0)
      # for years lower or equal to 2015 assume bunkers equal to JRC historical values
      y1 <- getYears(JRC)[getYears(JRC, as.integer = TRUE)<=2015]
      JRC_share[JRC_reg,y1,] <- JRC[JRC_reg,y1,"FE|Transport|LDV|Liquids (EJ/yr)"]/(JRC[JRC_reg,y1,"FE|Transport|non-LDV|Liquids (EJ/yr)"]+JRC[JRC_reg,y1,"FE|Transport|LDV|Liquids (EJ/yr)"])
      # for years after 2015 assume LDV share constant and eqaul to JRC 2015 values
      y2 <- getYears(share)[getYears(share, as.integer = TRUE)>2015]
      JRC_share[,y2,] <- JRC_share[,2015,]
      ## setting EU shares equal to JRC values
      varname_SSP2 <- getNames(share[,, "gdp_SSP2"])[1]
      share[JRC_reg,getYears(JRC_share), varname_SSP2] <- JRC_share[JRC_reg,getYears(JRC_share),]
      # redefining LDV and non-LDV liquids
      feTotal <- dimSums(result[,,c("fepet","fedie")],dim=3.2)
      feShares <- new.magpie(cells_and_regions = getRegions(share), years = intersect(getYears(share),getYears(result)), names = getNames(result[,,c("fepet","fedie")]))
      feShares[,,"fepet"] <- setNames(setNames(share[getRegions(share),getYears(feShares),"share_LDV_totliq"],"fepet"),NULL)
      feShares[,,"fedie"] <- (1-setNames(setNames(share[getRegions(share),getYears(feShares),"share_LDV_totliq"],"fepet"),NULL))
      feTransp <- new.magpie(cells_and_regions = getRegions(share), years = getYears(feShares), names = getNames(result[,,c("fepet","fedie")]))

      for (i in c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5", "SDP", "SDP_EI", "SDP_RC", "SDP_MC", "SSP2EU")) {
        i1 <- paste0(i, ".fepet")
        i2 <- paste0(i, ".fedie")
        feTransp[, getYears(feShares), i1] <- feShares[, getYears(feShares), i1] * setNames(feTotal[, getYears(feShares), i], i1)
        feTransp[, getYears(feShares), i2] <- feShares[, getYears(feShares), i2] * setNames(feTotal[, getYears(feShares), i], i2)
      }

      # extrapolating missing historical years
      result[,getYears(feTransp),getNames(feTransp)] <- feTransp[,getYears(feTransp),getNames(feTransp)]
    }

    if(subtype == "FE_buildings"){
      # Attribute the growth in water heating demand of the EDGE Region OCD to TUR,
      # and retrieve it from AUS, CAN, CHE (Swiss), NOR, NZL
      # For SSP1, SSP2 and SDP
      names_2_change = grep("(SSP1|SSP2|SDP|SDP_EI|SDP_RC|SDP_MC|SSP2EU).*water_heating", getNames(result), value = TRUE)
      names_2_change_elec = grep("elec",names_2_change,value = T)
      names_2_change_nonelec = grep("elec",names_2_change,value = T, invert = T)
      regs_OCD = c("AUS","CAN","CHE","NOR","NZL")
      reg_TUR = "TUR"
      end_of_history = 2015
      scenario_time = getYears(result, T)[getYears(result, T) > end_of_history]

      WH_growth = result[regs_OCD,scenario_time,names_2_change] -  dimReduce(result[regs_OCD,end_of_history,names_2_change])
      WH_growth[,,names_2_change_elec] = WH_growth[,,names_2_change_elec] * 0.5
      WH_growth[WH_growth < 0] <- 0
      WH_growth_agg = dimSums(WH_growth, dim = 1)

      result[getRegions(WH_growth), getYears(WH_growth), getNames(WH_growth)] <- result[getRegions(WH_growth), getYears(WH_growth), getNames(WH_growth)] - WH_growth
      result[reg_TUR, getYears(WH_growth), getNames(WH_growth)] <- result[reg_TUR, getYears(WH_growth), getNames(WH_growth)] + WH_growth_agg

    }



  } else if(subtype %in% c("Capital")){

    mappingfile <- toolGetMapping(type = "regional", name = "regionmappingEDGE.csv", returnPathOnly = TRUE, where = "mappingfolder")
    mapping <- read.csv2(mappingfile)
    region_col = which(names(mapping) == "RegionCodeEUR_ETP")
    iso_col = which(names(mapping) == "CountryCode")

    x = x[,getYears(x,T)[which(getYears(x,T) <= 2100)],]

    wg     <- calcOutput("GDP", aggregate=F)
    wfe    <- calcOutput("FEdemand", subtype = "FE", aggregate = F)

    getSets(wg) = gsub("variable","scenario",getSets(wg))
    getSets(wfe) = gsub("item","data", getSets(wfe))
    wg = add_dimension(wg,dim = 3.2, add = "data",nm ="kap")

    #***Reproduce this in the aggregation of CapitalUnit in calcCapital
    corres_ener_cap = c(kapal = "fealelb",
                        kapsc = "fescelb",
                        kaphc = "ueswb")
    wfe = do.call(mbind,
                  lapply(names(corres_ener_cap), function(kap_nm){
                    ener_nm = corres_ener_cap[kap_nm]
                    tmp = wfe[,,ener_nm]
                    getNames(tmp) = gsub(ener_nm,kap_nm,getNames(tmp))
                    return(tmp)
                  })
    )

    years_select = intersect(  intersect(getYears(x),getYears(wg))
                               ,getYears(wfe))

    wfe = wfe[,years_select,]
    wg = wg[,years_select,]

    weights = mbind(wfe,wg)

    x = toolAggregate(x[,years_select,],mappingfile, weight = weights[,,getNames(x)], from = region_col, to = iso_col )
    result = x

  } else if(subtype %in% c("CapitalUnit")){

    mappingfile <- toolGetMapping(type = "regional", name = "regionmappingEDGE.csv", returnPathOnly = TRUE, where = "mappingfolder")
    mapping <- read.csv2(mappingfile)
    region_col = which(names(mapping) == "RegionCodeEUR_ETP")
    iso_col = which(names(mapping) == "CountryCode")

    wg     <- NULL


    x = toolAggregate(x[,,],mappingfile, weight = wg, from = region_col, to = iso_col )
    result = x

  } else if(subtype %in% c("ES_buildings")){

    mappingfile <- toolGetMapping(type = "regional", name = "regionmappingEDGE.csv", returnPathOnly = TRUE, where = "mappingfolder")
    mapping <- read.csv2(mappingfile)
    region_col = which(names(mapping) == "RegionCodeEUR_ETP")
    iso_col = which(names(mapping) == "CountryCode")

    select_years = intersect(getYears(x,as.integer = T),rem_years_hist)
    wg <- calcOutput("GDP", years = select_years, aggregate = FALSE)

    # duplicate SSP2 for SSP2_lowEn an SSP2EU for Navigate scenarios
    wg <- mbind(
      wg,
      do.call("mbind", lapply(paste0("gdp_SSP2EU_NAV_", c("act", "tec", "ele", "lce", "all")),
                              function(navScen) {
                                setItems(wg[,, "gdp_SSP2EU"], 3, navScen)
                              })),
      setItems(wg[,, "gdp_SSP2"], 3, "gdp_SSP2_lowEn")
    )

    getNames(wg) = gsub("gdp_","", getNames(wg))

    x = toolAggregate(x[,select_years,],mappingfile, weight = wg[,,getNames(x,dim=1)], from = region_col, to = iso_col )
    result = x

  } else if (subtype == "Floorspace") {
    mappingfile <- toolGetMapping(type = "regional", name = "regionmappingEDGE.csv", returnPathOnly = TRUE, where = "mappingfolder")
    mapping <- read.csv2(mappingfile)
    region_col = which(names(mapping) == "RegionCodeEUR_ETP")
    iso_col = which(names(mapping) == "CountryCode")

    getNames(x) <- paste0("gdp_", getNames(x))
    wp <- calcOutput("Population", years = rem_years_hist, aggregate = FALSE)
    getSets(wp) <- gsub("variable", "scenario", getSets(wp))
    getItems(wp, "scenario") <- gsub("pop_", "gdp_", getItems(wp, "scenario"))
    # duplicate SSP2 for SSP2_lowEn an SSP2EU for Navigate scenarios
    wp <- mbind(
      wp,
      do.call("mbind", lapply(paste0("gdp_SSP2EU_NAV_", c("act", "tec", "ele", "lce", "all")),
                              function(navScen) {
                                setItems(wp[,, "gdp_SSP2EU"], 3, navScen)
                              })),
      setItems(wp[,, "gdp_SSP2"], 3, "gdp_SSP2_lowEn")
    )

    x <- toolAggregate(x[, rem_years_hist, ], mappingfile, weight = wp,
                       from = region_col, to = iso_col )
    result <- x
  }
  return(result)
}
