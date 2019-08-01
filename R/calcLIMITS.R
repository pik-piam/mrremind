#' @importFrom dplyr group_by_ summarise_ ungroup
#' @importFrom magclass as.magpie getNames getSets getRegions
#' @importFrom utils read.csv2
#' @importFrom quitte as.quitte


calcLIMITS <- function(VERBOSE=FALSE) {
  
  message <- function(itext) {cat(paste0(itext, "\n"))}
  
  #-- INITIALISATION ----------------
  if (VERBOSE) message(">> Initialization...")
  # local functions
  allocate_c2r_ef <- function(id_ef, ip_region, ip_country, ip_year, ip_scenario) {
    dummy                   <- id_ef[ip_region, ip_year, ip_scenario]             
    dummy[,,]               <- setCells(id_ef[ip_country, ip_year, ip_scenario], "GLO")
    #names(dimnames(dummy))  <- c("region", "years", "data1.data2.species.scenario")
    
    return(dummy)
  }
  
  allocate_min2r_ef <- function(id_ef, ip_region, ip_countryGroup, ip_year, ip_scenario) {
    
    dummy <- id_ef[ip_region, ip_year, ip_scenario]
    
    # Get minimum values across country group
    tmp <- as.quitte(id_ef[ip_countryGroup,ip_year,ip_scenario]) %>% 
      group_by_(~data1,~data2) %>% 
      summarise_(value=~min(value, na.rm=TRUE)) %>% 
      ungroup() %>% 
      as.data.frame() %>% 
      as.quitte() %>% 
      as.magpie()
    
    # Allocate minimum values to region
    dummy[ip_region, ip_year, ip_scenario] <- setYears(tmp)
    
    return(dummy)
  }
  
  # conversion factors 
  #TODO: should be centralised somewhere
  conv_ktSO2_to_ktS            <- 1/2     # 32/(32+2*16)
  conv_kt_per_PJ_to_Tg_per_TWa <- 1e-3 / (1e15/(365*24*60*60)*1e-12)
  
  # user-defined parameters
  time     <- seq(2005,2150,5)
  scenario <- c("SSP1","SSP2","SSP3","SSP4","SSP5","FLE") # These are additional scenarios to the CLE, SLE and MFR 
  # scenarios already included in the LIMITS data
  
  p_dagg_year <- 2005
  p_dagg_pop  <- "pop_SSP2"
  p_dagg_gdp  <- "gdp_SSP2"
  p_dagg_map  <- "regionmappingTIMER.csv"
  
  # list of OECD countries
  #TODO: may want to place this in a mapping file or in a R library
  r_oecd <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", 
              "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")
  
  
  #-- READ IN DATA ------------------
  if (VERBOSE) message(">> Read-in data...")
  # read in LIMITS data
  #  > activity data
  activities <- readSource("LIMITS", subtype="activities")
  getNames(activities) <- getNames(activities, dim=1)             # unit information not needed, causing some problems
  activities[is.na(activities)] <- 0       # set NA to 0
  #  > emission data
  emissions  <- readSource("LIMITS", subtype="emissions")
  emissions  <- collapseNames(emissions, collapsedim=2)
  emissions[is.na(emissions)]   <- 0       # set NA to 0
  
  # read in population and GDP data
  pop <- calcOutput("Population",aggregate=FALSE)[,p_dagg_year,p_dagg_pop]
  gdp <- calcOutput("GDPppp",    aggregate=FALSE)[,p_dagg_year,p_dagg_gdp]
  
  # calculate gdp per capita
  gdp_cap <- gdp/pop
  gdp_cap[is.na(gdp_cap)]   <- 0       # set NA to 0
  
  # read in sectoral mapping (LIMITS (TIMER) <> REMIND) #
  map_sectors       <- read.csv2(toolMappingFile("sectoral", "mappingLIMITStoLIMITSAggSectors.csv"), stringsAsFactors=TRUE)
  #map_REMINDSectors <- read.csv2(toolMappingFile("sectoral", "mappingLIMITSAggSectorstoREMIND.csv"), stringsAsFactors=TRUE)
  
  # read in regional map (select ISO and TIMER codes only)
  map_regions        <- read.csv2(toolMappingFile("regional", "regionmappingTIMER.csv"), stringsAsFactors=TRUE)[,c(2,4)] #
  names(map_regions) <- c("ISO3", "TIMER")
  
  
  #-- PROCESS DATA ------------------
  if (VERBOSE) message(">> Process data...")
  # Regional selections
  # select one country pertaining to WEU (all WEU countries should have the same EF). Used for SSP scenario rules
  select_weu <- paste(map_regions[which(map_regions$TIMER == "WEU")[1],1])
  # select one country pertaining to Western Africa. Used to allocate missing EFs in Eastern African countries
  select_waf <- paste(map_regions[which(map_regions$TIMER == "WAF")[1],1])
  # select Eastern African countries
  select_eaf <- paste(map_regions[which(map_regions$TIMER == "EAF"),   1])
  
#   # aggregate sectors
#   # "Unattributed" should not be used for modeling purposes (as indicated in the spreadsheet)
#   activities <- toolAggregate(activities, map_sectors, weight=NULL, dim=3.1)
#   emissions  <- toolAggregate(mselect(emissions, TIMER=map_sectors$LIMITS),  map_sectors, weight=NULL, dim=3.1)
  
  # restict data to Power and End-Use sectors
  activities <- activities[,, grep("Air", grep("End_Use|Power", map_sectors$LIMITS, value=TRUE), value=TRUE, invert=TRUE)]
  emissions  <- emissions[,,  grep("Air", grep("End_Use|Power", map_sectors$LIMITS, value=TRUE), value=TRUE, invert=TRUE)]
  
  # convert SO2 emission from TgSO2 to TgS
  emissions[,,"SO2"] <- emissions[,,"SO2"]*conv_ktSO2_to_ktS
  
  # calculate emission factors (only for power and end-use sectors) and convert from kt/PJ to Tg/Twa
  ef_limits                     <- emissions  /
    activities * 
    conv_kt_per_PJ_to_Tg_per_TWa
  ef_limits                     <- ef_limits[,,c("NOX", "CO", "VOC", "SO2", "BC", "OC")]     # remove additional NA species
  
  #ef_limits[is.na(ef_limits)]   <- 0                                                         # set NA to 0
  getSets(ef_limits)            <- c("region", "year", "data1", "data2", "data3")
  
  # allocate Benin EFs to Eastern African countries
  tmp                     <- ef_limits[select_waf,,]
  getRegions(tmp)         <- "GLO"                      # this is necessary for the allocation. 
  ef_limits[select_eaf,,] <- tmp                        # An alternative would be to use setCells
  
  # make output dummy "ef" which then has to be filled by the data
  ef <- do.call('mbind', 
                lapply(scenario, 
                       function(s) {new.magpie(getRegions(ef_limits), 
                                               c(2005,2010,2030,2050,2100), 
                                               gsub("CLE", s, getNames(ef_limits[,,"CLE"])))
                       }))
  
  # define country categories
  # low income countries (using World Bank definition < 2750 US$(2010)/Cap)
  r_L        <- dimnames(gdp_cap[getRegions(ef),,])$ISO3[which(gdp_cap[getRegions(ef),,] <= 2750)]
  # high and medium income countries
  r_HM       <- setdiff(getRegions(ef), r_L)
  # High-Medium income countries with strong pollution policies in place 
  r_HMStrong <- c("AUS", "CAN", "USA","JPN","FRA","GER","ESP")                       # FIXME which definition???
  r_HMStrong <- c("AND", "AUT", "BEL", "DNK", "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "ISL", "IRL", "ITA", "LIE", "LUX", "MLT", "MCO", "NLD", "NOR", "PRT", "SMR", "ESP", "SWE", "CHE", "GBR","JPN")                                     # Using JS definition for now
  # High-Medium income countries with lower emissions goals
  r_HMRest   <- setdiff(r_HM,r_HMStrong)
  
  # generate FLE and SSP scenarios
  # -------- Fix all scenarios to CLE in 2005 and 2010 ----------
  ef[,c(2005,2010),] <- ef_limits[,c(2005,2010),"CLE"]
  
  # ---------------- FLE ----------------------------------------
  # FLE: CLE 2010 emission factor is held constant
  ef[,,"FLE"] <- setYears(ef[,2010,"FLE"], NULL)     # NULL is actually the default value, skipping afterwards
  
  # ---------------- SSP1 ---------------------------------------
  # low income countries    
  ef[r_L,2030,"SSP1"]   <- ef_limits[r_L, 2030, "CLE"]                                                          # 2030: CLE30
  ef[r_L,2050,"SSP1"]   <- pmin(setYears(ef[r_L, 2030, "SSP1"]), 
                                setYears(allocate_c2r_ef(ef_limits, r_L, select_weu, 2030, "CLE")))             # 2050: CLE30 WEU, if not higher than 2030 value
  ef[r_L,2100,"SSP1"]   <- pmin(setYears(ef[r_L, 2050, "SSP1"]), setYears(ef_limits[r_L, 2030, "SLE"]))         # 2100: SLE30, if not higher than 2050 value
  # high income countries 
  ef[r_HM,2030,"SSP1"]  <- 0.75 * ef_limits[r_HM, 2030, "CLE"]                                                  # 2030: 75% of CLE30
  ef[r_HM,2050,"SSP1"]  <- pmin(setYears(ef[r_HM, 2030, "SSP1"]), setYears(ef_limits[r_HM, 2030, "SLE"]))       # 2050: SLE30, if not higher than 2030 value 
  ef[r_HM,2100,"SSP1"]  <- pmin(setYears(ef[r_HM, 2050, "SSP1"]), setYears(ef_limits[r_HM, 2030, "MFR"]))       # 2100: MFR, if not higher than 2050 value
  
  # ----------------- SSP2 --------------------------------------
  # High-Medium income countries with strong pollution policies in place
  ef[r_HMStrong,2030,"SSP2"] <- ef_limits[r_HMStrong,2030,"CLE"]                                                # 2030: CLE30
  ef[r_HMStrong,2050,"SSP2"] <- pmin(setYears(ef[r_HMStrong,       2030,"SSP2"]),
                                     setYears(ef_limits[r_HMStrong,2030,"SLE"]))                                # 2050: SLE30
  ef[r_HMStrong,2100,"SSP2"] <- pmin(setYears(ef[r_HMStrong,       2050,"SSP2"]),
                                     setYears(allocate_min2r_ef(ef_limits, r_HMStrong, r_oecd, 2030, "SLE")))   # 2100: Lowest SLE30 or lower
  # High-Medium income countries with lower emissions goals
  ef[r_HMRest,2030,"SSP2"]  <- ef_limits[r_HMRest,2030,"CLE"]                                                   # 2030: CLE30
  ef[r_HMRest,2050,"SSP2"]  <- pmin(setYears(ef[r_HMRest,       2030,"SSP2"]),
                                    setYears(allocate_min2r_ef(ef_limits, r_HMRest, r_HMRest, 2030, "CLE")))    # 2050: Min CLE30
  ef[r_HMRest,2100,"SSP2"]  <- pmin(setYears(ef[r_HMRest,2050,"SSP2"]),
                                    setYears(allocate_c2r_ef(ef_limits, r_HMRest, select_weu, 2030, "SLE")))    # 2100: SLE30 WEU  
  # low income countries
  ef[r_L,2030,"SSP2"]       <- setYears(ef_limits[r_L, 2020, "CLE"])                                            # 2030: CLE20
  ef[r_L,2050,"SSP2"]       <- pmin(setYears(ef[r_L,       2030,"SSP2"]),
                                    setYears(allocate_min2r_ef(ef_limits, r_L, r_L, 2030, "CLE")))              # 2050: Min CLE30
  ef[r_L,2100,"SSP2"]       <- pmin(setYears(ef[r_L,2050,"SSP2"]),
                                    setYears(allocate_c2r_ef(ef_limits, r_L, select_weu, 2030, "CLE")))         # 2100: CLE30 WEU
  
  # H-M-Strong:   2030 CLE30; 2050 SLE30;     2100 Lowest SLE30 or lower [EUR, JPN]                     = [3 5]
  # H-M-Rest:     2030 CLE30; 2050 Min CLE30; 2100 EUR SLE30             [CHN, LAM, MEA, ROW, RUS, USA] = [2 6 7 9 10 11]
  # Low:          2030 CLE20; 2050 Min CLE30; 2100 EUR CLE30             [AFR, IND, OAS]                = [1 4 8]
  
  # ----------------- SSP3 --------------------------------------
  # TODO
  
  # ----------------- SSP4 --------------------------------------
  # TODO
  
  # ----------------- SSP5 --------------------------------------
  # set SSP5 to the values of SSP1
  ef[,,"SSP5"] <- ef[,,"SSP1"]     
  
  # Further checks
  # set missing 2005 values to 2010 values
  tmp_reg_3rddim <- apply(ef[,2005,], 1, function (x) { return(is.na(x)) })
  dump <- lapply(colnames(tmp_reg_3rddim), function(x) {ef[x,2005,which(tmp_reg_3rddim[,x])] <<- setYears(ef[x,2010,which(tmp_reg_3rddim[,x])],2005)})
  rm(tmp_reg_3rddim, dump)
  
  # make sure that SSP2 values are always larger or equal than SSP1 values
  ef[,,"SSP2"] <- pmax(ef[,,"SSP2"], ef[,,"SSP1"])
  
  # filter all regions that are constant between 2030 and 2050 and continue to decline afterwards. Replace by linear interpolation between 2030 and 2100.
  tmp_reg_3rddim <- apply(ef[,c(2030,2050,2100),], 1, function (x) { return(x[1,] - x[2,] == 0.0 & (x[2,] - x[3,]) > 0) })
  dump <- lapply(colnames(tmp_reg_3rddim), function(x) {
    
    #slope     = (setYears(ef[x,2100,which(tmp_reg_3rddim[,x])],2050) - setYears(ef[x,2030,which(tmp_reg_3rddim[,x])],2050)) / 70.0
    #intercept = setYears(ef[x,2030,which(tmp_reg_3rddim[,x])],2050)-slope*2030
    ef[x,2050,which(tmp_reg_3rddim[,x])] <<- 
    #slope*2050 + intercept
    50/70*setYears(ef[x,2030,which(tmp_reg_3rddim[,x])],2050) + 20/70*setYears(ef[x,2100,which(tmp_reg_3rddim[,x])],2050)
      })
  rm(tmp_reg_3rddim, dump)
  
  # interpolate data (EFs and activities) over time. Remove y2000 from activities (this is the first time item hence -1)
  if (VERBOSE) message("  > Interpolate data over time...")
  ef         <- time_interpolate(ef, interpolated_year=time, integrate_interpolated_years=TRUE, extrapolation_type="constant")
  
#   # map data over REMIND sectors
#   if (VERBOSE) message("  > Map data over REMIND sectors...")
#   ef <- toolAggregate(ef, 
#                         map_REMINDSectors %>% unite(all, enty1, enty2, te, sector, sep="."), 
#                         weight=NULL, 
#                         dim=3.1)
  getSets(ef) <- c("region", "year", "sector.species.scenario")
  
  w <- calcOutput("Population",aggregate=FALSE)[getRegions(ef), 2005, "pop_SSP2"]
  #w <- new.magpie(getRegions(ef), 2005, getNames(ef, dim=1), fill=1)
  w <- setYears(w)
  getSets(w)[3] <- "sector"
  w[names(which(apply(ef[,2005,], 1, function (x) { return(all(is.na(x))) }))),,] <- 0.0
  
  return(list(x           = ef,
              weight      = w,
              unit        = "Tg(species)/TWa",
              description = "Emission factor trajectories of air pollutants (NOx, CO, VOC, SO2, BC, OC) over 2005-2100",
              note        = c('Emissions factors based on the LIMITS and SSP data')))
}
