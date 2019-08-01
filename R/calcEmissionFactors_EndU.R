#' @importFrom dplyr bind_rows group_by_ summarise_ ungroup mutate_ rename_ filter_ select_
#' @importFrom magclass as.magpie getCells getSets<- getNames<- getRegions<- mselect<- setNames
#' @importFrom readxl read_excel
#' @importFrom tidyr gather_
#' @importFrom utils read.csv read.csv2
#' @importFrom quitte as.quitte



calcEmissionFactors_EndU <- function(datasource="ECLIPSE", sectoral_resolution="aggregated", scenario_name="SSP2", VERBOSE=FALSE) {
  
  cat(paste("[Info] calcEmissionFactors_EndU is using the following scenario:", scenario_name, "\n"))
 
#  datasource="ECLIPSE"
#  sectoral_resolution="aggregated"
#  scenario_name="SSP2"
#  VERBOSE=FALSE
 
  if (datasource == "ECLIPSE") {
    if (sectoral_resolution == "aggregated") {
      
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
          summarise_(value=~ifelse(all(value == 0) , 0, min(value[value >0 ],na.rm= TRUE))) %>%  # a value 0 is often a sign for a NA that has been replaced with 0 for small countries
          ungroup() %>% 
          as.data.frame() %>% 
          as.quitte() %>% 
          as.magpie()
        
        # Allocate minimum values to region
        dummy[ip_region, ip_year, ip_scenario] <- setYears(tmp)
        
        return(dummy)
      }
      
      # not used
      fill_NAs_with_Emissions <- function(mdata, filldata, scenario = "CLE"){
        #index gives the set elements where all values in a region for a sector are NA
        index <- apply(mdata,c(1,3), function(x) {
          return(all(is.na(x)))
        })
        index <- as.magpie(index)
        index2 <- new.magpie(getCells(mdata),getYears(mdata),getNames(mdata))
        index2[,,] <- index
        index3 <- which(index2 == 1,arr.ind = TRUE)
        
        #filldata2 takes the structure of mdata and is filled with the values of filldata for the scenario chosen
        
        filldata2 <- new.magpie(getCells(mdata), getYears(mdata), getNames(mdata))
        if (!is.null(scenario)) filldata2[,,] <- collapseNames(filldata[,getYears(mdata),scenario][,,getNames(filldata2,dim = 1)])
        if (is.null(scenario))   filldata2[,,] <- collapseNames(filldata[,getYears(mdata),getNames(filldata2,dim = 1)])
        
        mdata[index3] <- filldata2[index3]
        
        return(mdata)
      }
      
      #not used
      getOriginalECLIPSEdata  <- function(subtype="activities.aggregated") {
        # Some sectors do not have any values or are not needed (remove them)
        rm_sectors = c("")
        #"AACID","CHEM","CHEMBULK","CUSM",
        #"Transformation_Coal", "Transformation_HLF", "Transformation_NatGas")
        #"Losses_Coal", "Losses_Distribution_Use", 
        rm_unwanted = c("Sum","Total","Unattributed")
        
        if (subtype == "activities.aggregated") {
          ap  <- read_excel("/mnt/input_preparation/sources/ECLIPSE/ACTIVITIES_EMF30_aggregated_Ev5a_Nov2015.xlsx", sheet="Air pollutants")
          
          ap <- ap %>% 
            gather_("year", "value", setdiff(colnames(ap),c("Region","Sector","Unit"))) %>% 
            mutate_(year=~as.numeric(paste(year))) %>% 
            rename_(region=~Region, sector=~Sector, unit=~Unit) %>% 
            mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
            filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region != "Global") %>% 
            select_(~-unit) %>% 
            as.data.frame()
          
          # Remove spurious data
          x <- ap %>% 
            filter_(~!(region == "Japan"  & sector == "End_Use_Residential_Coal")) %>%  # No activity data available
            filter_(~!(region == "Turkey" & sector == "End_Use_Transport_Coal"))        # No activity data available
          
        }
        
        if (subtype == "activities.extended") {
          ap  <- read_excel("/mnt/input_preparation/sources/ECLIPSE/ACTIVITIES_EMF30_extended_Ev5a_Nov2015.xlsx", sheet="Air pollutants")
          
          x <- ap %>% 
            gather_("year", "value", setdiff(colnames(ap),c("Region","Sector","Unit"))) %>% 
            mutate_(year=~as.numeric(paste(year))) %>% 
            rename_(region=~Region, sector=~Sector, unit=~Unit) %>% 
            mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
            filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region != "Global") %>% 
            select_(~-unit) %>% 
            as.data.frame()
          
        }
        
        if (subtype == "emissions.aggregated") {
          species <- c("SO2", "NOx", "VOC", "BC", "OC", "CO")
          
          cle <- do.call("bind_rows", 
                         lapply(species, 
                                function(s) {
                                  out <- read_excel("/mnt/input_preparation/sources/ECLIPSE/EMISSIONS_EMF30_aggregated_Ev5a_CLE_Nov2015.xlsx", sheet=s)
                                  out <- out%>%
                                    gather_("year", "value", setdiff(colnames(out),c("Region","Sector"))) %>% 
                                    mutate_(year=~as.numeric(paste(year))) %>% 
                                    rename_(region=~Region, sector=~Sector) %>% 
                                    mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
                                    mutate_(variable = ~s) %>% 
                                    filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region != "Global")
                                  
                                  return(out)
                                })) %>% 
            mutate_(scenario = ~"CLE")
          
          mfr <- do.call("bind_rows", 
                         lapply(species, 
                                function(s) {
                                  out <- read_excel("/mnt/input_preparation/sources/ECLIPSE/EMISSIONS_EMF30_aggregated_Ev5a_MFR_Nov2015.xlsx", sheet=s)
                                  out <- out%>%
                                    gather_("year", "value", setdiff(colnames(out),c("Region","Sector"))) %>% 
                                    mutate_(year=~as.numeric(paste(year))) %>% 
                                    rename_(region=~Region, sector=~Sector) %>% 
                                    mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
                                    mutate_(variable = ~s) %>% 
                                    filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region != "Global")
                                  return(out)
                                })) %>% 
            mutate_(scenario = ~"MFR")
          
          # MFR only has 2030 and 2050, so add CLE values for other years
          x <- bind_rows(cle, 
                         bind_rows(cle %>% filter_(~year %in% c(2000,2005,2010,2020)) %>% mutate_(scenario = ~"MFR"), mfr)) %>% 
            select_(~region,~year,~sector,~variable,~scenario,~value)
          
          # Remove spurious data
          x <- x %>% 
            filter_(~!(region == "Japan"  & sector == "End_Use_Residential_Coal")) %>%  # No activity data available
            filter_(~!(region == "Turkey" & sector == "End_Use_Transport_Coal"))        # No activity data available
          
        }
        
        if (subtype == "emissions.extended") {
          species <- c("SO2", "NOx", "VOC", "BC", "OC", "CO")
          
          cle <- do.call("bind_rows", 
                         lapply(species, 
                                function(s) {
                                  out <- read_excel("/mnt/input_preparation/sources/ECLIPSE/EMISSIONS_EMF30_aggregated_Ev5a_CLE_Nov2015.xlsx", sheet=s)
                                  out <- out %>%
                                    gather_("year", "value", setdiff(colnames(out),c("Region","Sector"))) %>% 
                                    rename_(region=~Region, sector=~Sector) %>% 
                                    mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
                                    mutate_(variable = ~s) %>% 
                                    filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region != "Global")
                                  return(out)
                                })) %>% 
            mutate_(scenario = ~"CLE")
          
          mfr <- do.call("bind_rows", 
                         lapply(species, 
                                function(s) {
                                  out <- read_excel("/mnt/input_preparation/sources/ECLIPSE/EMISSIONS_EMF30_aggregated_Ev5a_MFR_Nov2015.xlsx", sheet=s)
                                  out <- out%>%
                                    gather_("year", "value", setdiff(colnames(out),c("Region","Sector"))) %>% 
                                    rename_(region=~Region, sector=~Sector) %>% 
                                    mutate_(region = ~gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>% 
                                    mutate_(variable = ~s) %>% 
                                    filter_(~!sector %in% c(rm_sectors, rm_unwanted), ~region != "Global")
                                  return(out)
                                })) %>% 
            mutate_(scenario = ~"MFR")
          
          # MFR only has 2030 and 2050, so add CLE values for other years
          x <- bind_rows(cle, 
                         bind_rows(cle %>% filter_(~year %in% c(2000,2005,2010,2020)) %>% mutate_(scenario = ~"MFR"), mfr)) %>% 
            select_(~region,~year,~sector,~variable,~scenario,~value)
          
        }
        
        return(x)
      }
      
      #############################
      ##### Processing starts here
      #############################
      # conversion factors 
      #TODO: should be centralised somewhere
      conv_ktSO2_to_ktS            <- 1/2     # 32/(32+2*16)
      conv_kt_per_PJ_to_Tg_per_TWa <- 1e-3 / (1e15/(365*24*60*60)*1e-12)
      conv_kt_to_Tg  <- 1e-3
      conv_PJ_to_Twa <- (1e15/(365*24*60*60)*1e-12)
      
      # user-defined parameters
      time     <- c(seq(2005,2055,5), seq(2060,2110,10), 2130, 2150)
      scenario <- c("SSP1","SSP2","SSP3","SSP4","SSP5","FLE", "MFR", "CLE", "MFR_Transports", "GlobalEURO6", "FLE_building_transport", "SLCF_building_transport") # These are additional scenarios to the CLE and MFR 
      
      p_dagg_year <- 2005
      p_dagg_pop  <- "pop_SSP2"
      p_dagg_gdp  <- "gdp_SSP2"
      p_dagg_map  <- "regionmappingGAINS.csv"
      
      p_countryCategories <- "useGAINSregions"
      
      # list of OECD countries
      #TODO: may want to place this in a mapping file or in a R library
      r_oecd <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", 
                  "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

      
      #-- READ IN DATA ------------------
      if (VERBOSE) message(">> Read-in data...")
      # read in ECLIPSE data
      #  > activity data
      activities <- readSource("ECLIPSE", subtype=paste0("activities.", sectoral_resolution))
      activities <- activities[,c(2005,2010,2020,2030,2050),]
      #  > emission data
      emissions <- readSource("ECLIPSE", subtype=paste0("emissions.", sectoral_resolution))
      emissions <- emissions[,c(2005,2010,2020,2030,2050),]
      setsEmissions <- getSets(emissions)
      
      # read in sectoral mapping (ECLIPSE (IMAGE) <> EDGE)
      map_sectors_ECLIPSE2EDGE    <- read.csv(toolMappingFile("sectoral", "mappingECLIPSEtoEDGEsectors.csv"), stringsAsFactors=TRUE)

      # read in regional map (select ISO and GAINS codes only). This is required for the construction of the SSPs
      map_REMINDregions  <- read.csv2(toolMappingFile("regional", "regionmappingREMIND.csv"), stringsAsFactors=TRUE)
      map_regions  <- read.csv2(toolMappingFile("regional", p_dagg_map), stringsAsFactors=TRUE)[,c(2,3)] 
      map_regions  <- map_regions %>%  
        filter_(~CountryCode != "ANT") %>% # Remove Netherland Antilles (not in REMIND regional mapping)
        filter_(~RegionCode != "") %>% 
        mutate_(RegionCode = ~gsub("\\ \\+", "\\+", gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", RegionCode)))) %>% 
        mutate_(CountryCode = ~factor(CountryCode))
      
      # read in population and GDP data. required to compute gdp per cap
      pop <- calcOutput("Population",aggregate=FALSE)[,p_dagg_year,p_dagg_pop]
      gdp <- calcOutput("GDPppp",    aggregate=FALSE)[,p_dagg_year,p_dagg_gdp]
      
      
      #-- PROCESS DATA ------------------
      if (VERBOSE) message(">> Process data...")
      # set of sectors for which emission factors are computed 
      dimSector_EF   <- getNames(activities)[grepl("End_Use", getNames(activities)) & !grepl("End_Use_Transport", getNames(activities))]

      # Get specific sectors (transport and buildings)
      #transportNames <- getNames(activities)[grepl("End_Use_Transport", getNames(activities))]  # EFs are multiplied to activity levels directly in REMIND
      buildingNames  <- getNames(activities)[grepl("End_Use_Residential|End_Use_Services", getNames(activities))]
      
      # calculate gdp per capita
      gdp_cap <- gdp/pop
      gdp_cap[is.na(gdp_cap)]   <- 0       # set NA to 0
      
      # Regional selections
      # select one country pertaining to WEU (all WEU countries should have the same EF). Used for SSP scenario rules
      select_weu <- paste(map_regions[which(map_regions$RegionCode == "Western Europe")[1],1])
      
      # convert SO2 emission from TgSO2 to TgS 
      emissions[,,"SO2"] <- emissions[,,"SO2"]*conv_ktSO2_to_ktS
      
      # define missing SLE scenario (assumed to be 3/4 of the distance between CLE and MFR, according to discussion with Zig Klimont on 18th Feb 2016)
      cle = emissions[,,"CLE"]
      getNames(cle) = gsub("CLE", "MFR", getNames(cle))
      sle = cle - (cle - emissions[,,"MFR"])*0.75
      getNames(sle) = gsub("MFR", "SLE", getNames(sle))
      emissions=mbind(emissions, sle)
      rm(cle,sle)
      
      # calculate emission factors (only for power and end-use sectors, and not empty activities) and convert from kt/PJ to Tg/Twa
      ef_eclipse  <- emissions[,,dimSector_EF] / activities[,,dimSector_EF] * conv_kt_per_PJ_to_Tg_per_TWa
      
      getSets(ef_eclipse) <- c("region", "year", "data1", "data2", "data3")
      
      # some regions/countries have NA values everywhere. Allocate EF of the region to which they belong (except for Antartica)
      #NAregions <- which(sapply(getRegions(which(is.na(ef_eclipse), arr.ind = T)), function(k) all(is.na(as.numeric(ef_eclipse[k,,])))))
      NAregions <- c("AIA", "ATF", "BVT", "CCK", "COK", "CXR", "ESH", "FLK", "GIB", "GLP", "GUF",
                     "HMD", "IOT", "MSR", "MTQ", "MYT", "NFK", "NIU", "NRU", "PCN",
                     "REU", "SGS", "SHN", "SJM", "SPM", "TKL", "TWN", "UMI", "VAT", "VGB", "WLF")
      MissingRegions <- c("ALA", "BES", "BLM", "CUW", "GGY", "IMN", "JEY", "MAF", "PSE", "SSD", "SXM")
      AssociatedGAINSregions <- c("Western Europe", "Rest Central America", "Rest Central America", "Rest Central America", "Western Europe", "Western Europe", "Western Europe", 
                                  "Rest Central America", "Middle East", "Northern Africa", "Rest Central America")
      ef_eclipse["ATA",,] = 0    # Antartica -> 0
      for (kregi in NAregions) {
        subsitute_region = map_regions$CountryCode[map_regions$RegionCode == map_regions$RegionCode[map_regions$CountryCode == kregi] & !map_regions$CountryCode %in% c(NAregions, MissingRegions)][1]
        tmp <- ef_eclipse[subsitute_region,,]
        getRegions(tmp) <- kregi
        ef_eclipse[kregi,,] <- tmp
      }
      # some regions have no population data when disaggregating. 
      for (kregi in MissingRegions) {
        substitute_region = map_regions$CountryCode[map_regions$RegionCode == AssociatedGAINSregions[which(MissingRegions == kregi)] & 
                                                      !map_regions$CountryCode %in% MissingRegions][1]
        tmp <- ef_eclipse[substitute_region,,]
        getRegions(tmp) <- kregi
        ef_eclipse[kregi,,] <- tmp
      }
      
      # for the remaining NAs just set EF to 0 (activity levels are 0)
      ef_eclipse[is.na(ef_eclipse)]   <- 0                                                  
      rm(NAregions, MissingRegions, AssociatedGAINSregions)
      
      #   # Check ef corresponds to initial data
      #   data = ef_eclipse[,2010,] %>% 
      #     as.data.frame() %>% 
      #     select(-Cell, -Year) %>% 
      #     rename(country=Region, proc=Value) %>% 
      #     unite(data, Data1, Data2, Data3, sep=".") %>% 
      #     mutate(region = paste(sapply(paste(country), function(k) map_regions$RegionCode[map_regions$CountryCode == k])))
      #   
      #   check = inner_join(
      #     getOriginalECLIPSEdata("activities.aggregated") %>% filter(year == 2010, sector %in% dimSector_EF) %>% rename(activity = value) %>% select(-year),
      #     getOriginalECLIPSEdata("emissions.aggregated") %>% filter(year == 2010, sector %in% dimSector_EF) %>% rename(emission = value) %>% select(-year),
      #     by=c("region", "sector")
      #   ) %>% 
      #     mutate(emission=ifelse(variable == "SO2", emission*conv_ktSO2_to_ktS, emission)) %>% 
      #     unite(data, sector, variable, scenario, sep=".") %>% 
      #     mutate(ef = emission/activity*conv_kt_per_PJ_to_Tg_per_TWa) 
      #   
      #   View(inner_join(data, check, by=c("region", "data")) %>% 
      #          select(country,region,data,emission,activity,ef,proc) %>% 
      #          mutate(abs_diff=proc-ef, rel_diff=(proc-ef)/ef*100))
      
      
      # make output dummy "ef" and "emi" which then has to be filled by the data
      ef <- do.call('mbind', 
                    lapply(scenario, 
                           function(s) {new.magpie(getRegions(ef_eclipse), 
                                                   c(2005,2010,2030,2050,2100), 
                                                   gsub("CLE", s, getNames(ef_eclipse[,,"CLE"])))
                           }))
      
      
      # define country categories
      if (p_countryCategories == "perCountry") {
        # low income countries (using World Bank definition < 2750 US$(2010)/Cap)
        r_L        <- dimnames(gdp_cap[getRegions(ef),,])$ISO3[which(gdp_cap[getRegions(ef),,] <= 2750)]
        # high and medium income countries
        r_HM       <- setdiff(getRegions(ef), r_L)
        # High-Medium income countries with strong pollution policies in place 
        r_HMStrong <- c("AUS", "CAN", "USA","JPN")                       # FIXME which definition???
        # High-Medium income countries with lower emissions goals
        r_HMRest   <- setdiff(r_HM,r_HMStrong)
      } else {
        # Compute mean GDP/Cap per GAINS region
        regionMean_gdppcap <- sapply(unique(map_regions$RegionCode), function(x) {mean(gdp_cap[map_regions$CountryCode[map_regions$RegionCode == x],,])})
        
        # low income countries (using World Bank definition < 2750 US$(2010)/Cap)
        r_L        <- map_regions$CountryCode[map_regions$RegionCode %in% names(regionMean_gdppcap[regionMean_gdppcap <= 2750])]
        # high and medium income countries
        r_HM       <- setdiff(getRegions(ef), r_L)
        # High-Medium income countries with strong pollution policies in place 
        r_HMStrong <- map_regions$CountryCode[map_regions$RegionCode %in% c("Western Europe", "Japan")]   # FIXME definition taken from JeS matlab script
        # High-Medium income countries with lower emissions goals
        r_HMRest   <- setdiff(r_HM,r_HMStrong)
      }
      
      # generate FLE and SSP scenarios
      # -------- Fix all scenarios to CLE in 2005 and 2010 ----------
      ef[,c(2005,2010),]  <- ef_eclipse[,c(2005,2010),"CLE"]
      
      # ---------------- FLE ----------------------------------------
      # FLE: CLE 2010 emission factors and emissions are held constant
      ef[,,"FLE"]  <- setYears(ef[,2010,"FLE"], NULL)     # NULL is actually the default value, skipping afterwards
      
      # ---------------- SSP1 ---------------------------------------
      # Emission factors
      # low income countries   
      ef[r_L,2030,"SSP1"]   <- ef_eclipse[r_L, 2030, "CLE"]                                                          # 2030: CLE30
      ef[r_L,2050,"SSP1"]   <- pmin(setYears(ef[r_L, 2030, "SSP1"]), 
                                    setYears(allocate_c2r_ef(ef_eclipse, r_L, select_weu, 2030, "CLE")))             # 2050: CLE30 WEU, if not higher than 2030 value
      ef[r_L,2100,"SSP1"]   <- pmin(setYears(ef[r_L, 2050, "SSP1"]), setYears(ef_eclipse[r_L, 2030, "SLE"]))         # 2100: SLE30, if not higher than 2050 value
      # high income countries 
      ef[r_HM,2030,"SSP1"]  <- 0.75 * ef_eclipse[r_HM, 2030, "CLE"]                                                  # 2030: 75% of CLE30
      ef[r_HM,2050,"SSP1"]  <- pmin(setYears(ef[r_HM,  2030, "SSP1"]), setYears(ef_eclipse[r_HM, 2030, "SLE"]))      # 2050: SLE30, if not higher than 2030 value 
      ef[r_HM,2100,"SSP1"]  <- pmin(setYears(ef[r_HM,  2050, "SSP1"]), setYears(ef_eclipse[r_HM, 2030, "MFR"]))      # 2100: MFR, if not higher than 2050 value

      # ----------------- SSP2 --------------------------------------
      # Emission factors
      # High-Medium income countries with strong pollution policies in place
      ef[r_HMStrong,2030,"SSP2"] <- ef_eclipse[r_HMStrong,2030,"CLE"]                                                # 2030: CLE30
      ef[r_HMStrong,2050,"SSP2"] <- pmin(setYears(ef[r_HMStrong,        2030,"SSP2"]),
                                         setYears(ef_eclipse[r_HMStrong,2030,"SLE"]))                                # 2050: SLE30
      ef[r_HMStrong,2100,"SSP2"] <- pmin(setYears(ef[r_HMStrong,        2050,"SSP2"]),
                                         setYears(allocate_min2r_ef(ef_eclipse, r_HMStrong, r_oecd, 2030, "SLE")))   # 2100: Lowest SLE30 or lower
      # High-Medium income countries with lower emissions goals
      ef[r_HMRest,2030,"SSP2"]  <- ef_eclipse[r_HMRest,2030,"CLE"]                                                   # 2030: CLE30
      ef[r_HMRest,2050,"SSP2"]  <- pmin(setYears(ef[r_HMRest,       2030,"SSP2"]),
                                        setYears(allocate_min2r_ef(ef_eclipse, r_HMRest, r_HMRest, 2030, "CLE")))    # 2050: Min CLE30
      ef[r_HMRest,2100,"SSP2"]  <- pmin(setYears(ef[r_HMRest,2050,"SSP2"]),
                                        setYears(allocate_c2r_ef(ef_eclipse, r_HMRest, select_weu, 2030, "SLE")))    # 2100: SLE30 WEU  
      # low income countries
      ef[r_L,2030,"SSP2"]       <- setYears(ef_eclipse[r_L, 2020, "CLE"])                                            # 2030: CLE20
      ef[r_L,2050,"SSP2"]       <- pmin(setYears(ef[r_L,       2030,"SSP2"]),
                                        setYears(allocate_min2r_ef(ef_eclipse, r_L, r_L, 2030, "CLE")))              # 2050: Min CLE30
      ef[r_L,2100,"SSP2"]       <- pmin(setYears(ef[r_L,2050,"SSP2"]),
                                        setYears(allocate_c2r_ef(ef_eclipse, r_L, select_weu, 2030, "CLE")))         # 2100: CLE30 WEU
      
      # H-M-Strong:   2030 CLE30; 2050 SLE30;     2100 Lowest SLE30 or lower [EUR, JPN]                     = [3 5]
      # H-M-Rest:     2030 CLE30; 2050 Min CLE30; 2100 EUR SLE30             [CHN, LAM, MEA, ROW, RUS, USA] = [2 6 7 9 10 11]
      # Low:          2030 CLE20; 2050 Min CLE30; 2100 EUR CLE30             [AFR, IND, OAS]                = [1 4 8]
      
      # ----------------- SSP3 --------------------------------------
      # TODO
      
      # ----------------- SSP4 --------------------------------------
      # TODO
      
      # ----------------- SSP5 --------------------------------------
      # set SSP5 to the values of SSP1
      ef[,,"SSP5"]  <- ef[,,"SSP1"]     
      
      # Find occurences where the EF path is not monotonously decreasing  
      #   for (kregi in getRegions(ef)) {
      #     for (kdata in getNames(ef)) {
      #       
      #       y1 = ef[kregi,2005,kdata] %>% as.numeric()
      #       y2 = ef[kregi,2010,kdata] %>% as.numeric()
      #       y3 = ef[kregi,2030,kdata] %>% as.numeric()
      #       y4 = ef[kregi,2050,kdata] %>% as.numeric()
      #       y5 = ef[kregi,2100,kdata] %>% as.numeric()
      #       
      #       if (y1 > y2 || y2 > y3 || y3 > y4 || y4 > y5) {
      #         print(paste0(kregi, ": ", kdata))
      #       }
      #     }
      #     stop()
      #   }
      
      # make sure that SSP2 is always higher than SSP1 (and SSP5)
      # Takes toooooooooooooo much time (~1h30). commented out for now
      #   for (kregi in getRegions(ef)) {
      #     for (kssp1 in getNames(ef[,,"SSP1"])) {
      #       
      #       kssp2 = paste0(strsplit(kdata, ".", fixed=TRUE)[[1]][1], ".", strsplit(kdata, ".", fixed=TRUE)[[1]][2], ".SSP2")
      #       
      #       for (kyear in getYears(ef)) {
      #         y1 = ef[kregi,kyear,kssp1] %>% as.numeric()
      #         y2 = ef[kregi,kyear,kssp2] %>% as.numeric()
      #         
      #         if (y1 > y2) {
      #           ef[kregi,kyear,kssp2] = y1
      #         }
      #       }
      #     }
      #   }
      #   for (kregi in getRegions(emi)) {
      #     for (kssp1 in getNames(emi[,,"SSP1"])) {
      #       
      #       kssp2 = paste0(strsplit(kdata, ".", fixed=TRUE)[[1]][1], ".", strsplit(kdata, ".", fixed=TRUE)[[1]][2], ".SSP2")
      #       
      #       for (kyear in getYears(emi)) {
      #         y1 = emi[kregi,kyear,kssp1] %>% as.numeric()
      #         y2 = emi[kregi,kyear,kssp2] %>% as.numeric()
      #         
      #         if (y1 > y2) {
      #           emi[kregi,kyear,kssp2] = y1
      #         }
      #       }
      #     }
      #   }
      
      # filter all regions and sectors that are constant between 2030 and 2050 and continue to decline afterwards. Replace by linear interpolation
      # between 2030 and 2100
      
      
      # ----------------- CLE and MFR -------------------------------
      ef[,c(2005,2010,2030,2050),c("CLE","MFR")] <- ef_eclipse[,c(2005,2010,2030,2050),c("CLE","MFR")]
      ef[,2100,c("CLE","MFR")] <- setYears(ef_eclipse[,2050,c("CLE","MFR")])                           # for 2100, take the same values as in 2050

      #!! Transport EFs are used directly in REMIND !!
      # ---------------- Global EURO6 for Transports -----------------------------
      ef[,c(2030,2050,2100),"GlobalEURO6"] <- ef[,c(2030,2050,2100),"SSP2"]
      #ef[,c(2030,2050,2100),"GlobalEURO6"][,,transportNames] <- setCells(ef["FRA",c(2030,2050,2100),"GlobalEURO6"][,,transportNames], "GLO")
      
      #emi[,c(2030,2050,2100),"GlobalEURO6"] <- emi[,c(2030,2050,2100),"SSP2"]  
      
      # ---------------- MFR Transports -----------------------------
      ef[,c(2030,2050,2100),"MFR_Transports"] <- ef[,c(2030,2050,2100),"SSP2"]
      #mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "MFR_Transports") = mselect(ef,year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "MFR")
      
      #emi[,c(2030,2050,2100),"MFR_Transports"] <- emi[,c(2030,2050,2100),"SSP2"]
      
      # ---------------- FLE_building_transport ------------------------------
      ef[,c(2030,2050,2100),"FLE_building_transport"] <- ef[,c(2030,2050,2100),"SSP2"]
      mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data3 = "FLE_building_transport")  = mselect(ef,year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data3 = "FLE")
      #mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "FLE_building_transport") = mselect(ef,year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "FLE")
      
      # ---------------- SLCF_building_transport ------------------------------
      ef[,c(2030,2050,2100),"SLCF_building_transport"] <- ef[,c(2030,2050,2100),"SSP2"]
      mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data2=c("BC", "OC"), data3 = "SLCF_building_transport")  = mselect(ef,year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data2=c("BC", "OC"), data3 = "FLE")
      #mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data2=c("BC", "OC"), data3 = "SLCF_building_transport") = mselect(ef,year = c("y2030", "y2050", "y2100"), data1 = transportNames, data2=c("BC", "OC"), data3 = "FLE")
      
      # ----- Aggregate back to REMIND regions (to speed up processing)
      emiNam <-getNames(ef,TRUE)[2:3]
      newdim <- apply(
        sapply(
          do.call("expand.grid", emiNam),as.character),
        1,paste, collapse = ".")
      
      x = collapseNames(ef[,,scenario_name], collapsedim = 3.3)
      
      activities.EF <- do.call('mbind', 
                               lapply(newdim, 
                                      function(scen) {setNames(activities[,,dimSector_EF], paste(getNames(activities[,,dimSector_EF]), scen, sep = "."))}))
      y = collapseNames(activities.EF[,,scenario_name], collapsedim = 3.3)[,2020,,invert=TRUE]
      y = mbind(y, setYears(y[,2050,], 2100)) # Use EDGE info to extrapolate
 
      z = toolAggregate(x, 
                          map_sectors_ECLIPSE2EDGE[which(map_sectors_ECLIPSE2EDGE[,2] %in% getNames(x, dim=1) & !duplicated(map_sectors_ECLIPSE2EDGE[,2])),c(2,3)], 
                          weight=y, dim=3.1)
      
      getSets(z) <- c("region", "year", "sector.species")
      
    }
  }
  
  return(list(x=z,
              weight=NULL,
              unit="Mt",
              description="End-Use emissions"))
}
