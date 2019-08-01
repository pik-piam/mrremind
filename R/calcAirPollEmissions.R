calcAirPollEmissions <- function() {
  #-- INITIALISATION ----------------
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
  # read in LIMITS data
  #  > emission data
  emissions  <- readSource("LIMITS", subtype="emissions")
  emissions  <- collapseNames(emissions, collapsedim=2)
  emissions[is.na(emissions)]   <- 0       # set NA to 0
  
  # read in population and GDP data
  pop <- calcOutput("Population",aggregate=FALSE)[,p_dagg_year,p_dagg_pop]
  
  # read in sectoral mapping (LIMITS (TIMER) <> REMIND)
  map_sectors       <- read.csv2(toolMappingFile("sectoral", "mappingLIMITStoLIMITSAggSectors.csv"), stringsAsFactors=TRUE)
  map_REMINDSectors <- read.csv2(toolMappingFile("sectoral", "mappingLIMITSAggSectorstoREMIND.csv"), stringsAsFactors=TRUE)
  
  # read in regional map (select ISO and TIMER codes only)
  map_regions        <- read.csv2(toolMappingFile("regional", "regionmappingTIMER.csv"), stringsAsFactors=TRUE)[,c(2,4)]
  names(map_regions) <- c("ISO3", "TIMER")
  
  
  #-- PROCESS DATA ------------------
  # Regional selections
  # select one country pertaining to WEU (all WEU countries should have the same EF). Used for SSP scenario rules
  select_weu <- paste(map_regions[which(map_regions$TIMER == "WEU")[1],1])
  # select one country pertaining to Western Africa. Used to allocate missing EFs in Eastern African countries
  select_waf <- paste(map_regions[which(map_regions$TIMER == "WAF")[1],1])
  # select Eastern African countries
  select_eaf <- paste(map_regions[which(map_regions$TIMER == "EAF"),   1])
  
  # aggregate sectors
  # "Unattributed" should not be used for modeling purposes (as indicated in the spreadsheet)
  # activities <- toolAggregate(activities, map_sectors, weight=NULL, dim=3.1)
  emissions  <- toolAggregate(mselect(emissions, TIMER=map_sectors$LIMITS), map_sectors, weight=NULL, dim=3.1)
  
  # convert SO2 emission from TgSO2 to TgS
  emissions[,,"SO2"] <- emissions[,,"SO2"]*conv_ktSO2_to_ktS
  
  # get emissions for sectors which are not represented in REMIND (indprocess, solvents, extraction)
  em_limits  <- emissions[,, unique(map_REMINDSectors$LIMITS), invert=TRUE]
  getNames(em_limits, dim=1)  <- c("indprocess", "solvents", "extraction")
  
  # make output dummy "ef" which then has to be filled by the data
  em <- do.call('mbind', 
                lapply(scenario, 
                       function(s) {new.magpie(getRegions(em_limits), 
                                               c(2005,2010,2030,2050,2100), 
                                               gsub("CLE", s, getNames(em_limits[,,"CLE"])))
                       }))
  getSets(em)[1] <- "region" 
  getSets(em)[2] <- "period" 
  getSets(em)[3] <- "sector.species.scenario"
  
  # interpolate data over time. Remove y2000 from activities (this is the first time item hence -1)
  em         <- time_interpolate(em, interpolated_year=time, integrate_interpolated_years=TRUE, extrapolation_type="constant")

  # compute weights
  w <- setYears(pop[,2005,])
  
  return(list(x           = em,
              weight      = w,
              unit        = "Tg(species)",
              description = "Exogenous emissions trajectories of air pollutants (NOx, CO, VOC, SO2, BC, OC) from the solvents and industrial processes sectors over 2005-2100",
              note        = c('Exogenous emissions of solvents and industrial processes taken from the LIMITS WP4 project')))
}