calcCapital <- function(subtype = "Capital") {

  #--- Parameters ---
  #***Reproduce this in the aggregation of Capital in convertEDGE
  corres_ener_cap = c(kapal = "fealelb",
                      kapsc = "fescelb",
                      kaphc = "ueswb")
  #---

  if (subtype == "Capital") {
    # get capital stocks for EDGE sectors
    cap_macro <- readSource("EDGE",subtype = subtype)
    millionDol2trillionDol <- 1e-6

    additional_years <- seq(2105, 2150, 5)
    cap_macro <- time_interpolate(cap_macro,
                                  additional_years,
                                  integrate_interpolated_years = TRUE,
                                  extrapolation_type = "constant")

    # compute macroeconomic capital stock based on capital intensities from PWT and ssp scenarios
    # t.b.d.: correct for capital stock part that enters energy sectors
    capital <- readSource("PWT")[, , "rkna"]
    getNames(capital) <- "kap"
    capital[is.na(capital)] <- 0
    gdpppp_hist <- calcOutput("GDPPast", GDPPast = "PWT", aggregate = FALSE)
    #pop = calcOutput("Population", aggregate = F)
    cap_intensity <- capital / setNames(gdpppp_hist, NULL)


    #use initial gdp as in REMIND which differs from PWT
    gdpppp_hist = calcOutput("GDPPast", aggregate = FALSE)
    gdpppp <- calcOutput("GDP", aggregate = FALSE, years = seq(2005, 2150, 5))
    #getNames(gdpppp) <- sub("gdp_","",getNames(gdpppp))
    my_scen <- c("gdp_SSP1", "gdp_SSP2", "gdp_SSP3", "gdp_SSP4", "gdp_SSP5", "gdp_SSP2EU",
                 "gdp_SDP", "gdp_SDP_EI", "gdp_SDP_RC", "gdp_SDP_MC")
    gdpppp <- mselect(gdpppp, variable = my_scen)

    p41 <- setYears(cap_intensity[,rep(1,32),],seq(1995,2150,5))
    p41 <- add_dimension(p41, dim=3.1, add="ssp",nm=my_scen)
    cap_intensity_future <- p41
    convtime <- p41
    gdp_weight <- p41

    #ssp variation
    convtime[,,"gdp_SSP1"]   = 150
    convtime[,,"gdp_SSP2"]   = 250
    convtime[,,"gdp_SSP3"]   = 500
    convtime[,,"gdp_SSP4"]   = 300
    convtime[,,"gdp_SSP5"]   = 150
    convtime[,,"gdp_SDP"]    = 150
    convtime[,,"gdp_SDP_EI"] = 150
    convtime[,,"gdp_SDP_RC"] = 150
    convtime[,,"gdp_SDP_MC"] = 150
    convtime[,,"gdp_SSP2EU"]   = 250

    for (t in c("y1995","y2000","y2005")){
      cap_intensity_future[,t,] <- cap_intensity[,t,]
      gdp_weight[,t,] <- gdpppp_hist[,t,]
    }
    cap_intensity_ref = cap_intensity["JPN","y2010"]
    getRegions(cap_intensity_ref) <- "GLO"
    lambda=0
    for(t in getYears(gdpppp)) {
        cap_intensity_future[,t,] = ((convtime[,t,]-lambda) * collapseNames(setYears(cap_intensity[,"y2010",])) +                      +
                  lambda* setNames(setYears(cap_intensity_ref,NULL),NULL))/convtime[,t,]
        lambda =lambda+5
        gdp_weight[,t,] <- gdpppp[,t,]
    }
    cap_intensity_future[is.na(cap_intensity_future)] <- 0
    cap_int_new <- cap_intensity_future
    for(t in getYears(cap_intensity_future)) {
      for(r in getRegions(cap_intensity_future)) {
      if(cap_intensity_future[r,t,"gdp_SSP2"]==0) {
        # get current mapping
        map <- toolGetMapping(type = "regional", name = getConfig("regionmapping"), where = "mappingfolder")
        # get list of countries that belong to the same region as r
        regi   <- map$RegionCode[map$CountryCode==r]
        c_regi <- map$CountryCode[map$RegionCode==regi]
        # filter out the regions that are 0
        c_regi <- c_regi[!cap_intensity_future[c_regi,t,"gdp_SSP2"]==0]
        # generate mapping for the aggregation
        mapping            <- map[which(map$CountryCode %in% c_regi),]
        mapping$RegionCode <- r
        # store calculated data in separate file
        cap_int_new[r,t,] <- toolAggregate(cap_intensity_future[c_regi,t,],mapping,weight=gdp_weight[c_regi,t,])
      }
      }
    }

    cap_future <- cap_int_new * gdp_weight
    y = intersect(getYears(cap_future), getYears(cap_macro))


    # Add SSP2EU and SDP scenarios
    cap_macro_SSP2A <- cap_macro[,,"gdp_SSP2"]
    getNames(cap_macro_SSP2A) <- gsub("SSP2", "SSP2EU", getNames(cap_macro_SSP2A))
    cap_macro <- mbind(cap_macro, cap_macro_SSP2A)

    cap_macro_SDP <- cap_macro[,,"gdp_SSP1"]
    for (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
      getNames(cap_macro_SDP) <- gsub("SSP1", i, getNames(cap_macro[,,"gdp_SSP1"]))
      cap_macro <- mbind(cap_macro, cap_macro_SDP)
    }


    cap_macro = mbind(cap_macro[,y,], cap_future[,y,])

    cap_macro <- cap_macro * millionDol2trillionDol

    # add industry subsectors energy efficiency capital stocks ----

    kap <- cap_macro %>%
      `[`(,2015,'gdp_SSP2EU.kap') %>%
      quitte::magclass_to_tibble(c('iso3c', NA, NA, NA, 'kap')) %>%
      dplyr::select('iso3c', 'kap')

    EEK <- calcOutput('Industry_EEK', kap = kap, supplementary = FALSE,
                      aggregate = FALSE, years = getYears(cap_macro))

    # tie outputs together ----
    output <- list(
      x = mbind(cap_macro, EEK),
      weight = NULL,
      unit = "trillion 2005US$",
      description = "Capital stock at constant 2005 national prices")
  }
  else if( subtype == "CapitalUnit") {

    data = readSource("EDGE", subtype = subtype)

    wfe    <- calcOutput("FEdemand", subtype = "FE", aggregate = F)[,2015,"gdp_SSP2"]
    wfe  = collapseNames(wfe)
    getSets(wfe) = gsub("item","variable", getSets(wfe))


    wfe_kap = do.call(mbind,
                  lapply(names(corres_ener_cap), function(kap_nm){
                    ener_nm = corres_ener_cap[kap_nm]
                    tmp = wfe[,,ener_nm]
                    getNames(tmp) = gsub(ener_nm,kap_nm,getNames(tmp))
                    return(tmp)
                  })
    )
    wg_prep = mbind(wfe[,,corres_ener_cap],
                    wfe_kap)

    # Apply the structure of data to the weights to avoid issues with multiplication
    weights = data
    weights[] <- NA
    for (item in getNames(wg_prep)){
      for (type in getNames(weights[,,item], dim = "type")){
        weights[,,item][,,type] <- wg_prep[,,item]
      }
    }

    output = list(x=data,weight=weights,
                  unit = "kWh for energy, $ for capital",
                  description = "Technology units with capital and energy properties")
  }

  return(output)
}
