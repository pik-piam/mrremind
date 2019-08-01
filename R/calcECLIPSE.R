#' @importFrom dplyr group_by_ summarise_ ungroup mutate_ rename_ filter_ select_
#' @importFrom magclass as.magpie getCells getSets<- getNames<- getSets getRegions<- mselect<- setNames write.magpie
#' @importFrom tidyr gather_
#' @importFrom utils read.csv read.csv2



calcECLIPSE <- function(sectoral_resolution="aggregated") {
  
  #-- INITIALISATION ----------------
  vcat(2,">> Initialization...\n")
  # local functions

  # conversion factors 
  #TODO: should be centralised somewhere
  conv_ktSO2_to_ktS            <- 1/2     # 32/(32+2*16)
  conv_kt_per_PJ_to_Tg_per_TWa <- 1e-3 / (1e15/(365*24*60*60)*1e-12)

  p_dagg_map  <- "regionmappingGAINS.csv"
  
  # set of sectors for which no emission factor will be computed (because there is no activity reported, or not in terms of energy)  
  dimSector_skipEF = c("AACID", "CEMENT", "CHEM", "CHEMBULK", "CUSM", "NACID", "PAPER", "STEEL",
                       "Losses_Coal", "Losses_Distribution_Use", "Losses_Vent_Flare",
                       "Transformations_Coal", "Transformations_HLF", "Transformations_HLF_Refinery", "Transformations_LLF", "Transformations_NatGas")

  dimSector_skipEF_edge = c("End_Use_Industry_Bio_Trad", "End_Use_Industry_Coal", "End_Use_Industry_HLF", "End_Use_Industry_LLF",
                            "End_Use_Industry_NatGas", "End_Use_Residential_Bio_Mod", "End_Use_Residential_Bio_Trad", "End_Use_Residential_Coal",
                            "End_Use_Residential_HLF", "End_Use_Residential_LLF", "End_Use_Residential_NatGas", "End_Use_Services_Bio_Trad",
                            "End_Use_Services_Coal")
  dimSector_skipEF_edge = c("")       # is this a bug or preliminary or what?
  
  #-- READ IN DATA ------------------
  vcat(2,">> Read-in data... \n")
  # read in ECLIPSE data
  #  > activity data
  activities <- readSource("ECLIPSE", subtype=paste0("activities.", sectoral_resolution))
  activities <- activities[,c(2005,2010,2020,2030,2050),]
  #  > emission data
  emissions <- readSource("ECLIPSE", subtype=paste0("emissions.", sectoral_resolution))
  emissions <- emissions[,c(2005,2010,2020,2030,2050),]
  setsEmissions <- getSets(emissions)
  
  # read in sectoral mapping (ECLIPSE (IMAGE) <> REMIND) 
  map_sectors_ECLIPSE2Agg    <- read.csv(toolMappingFile("sectoral", "mappingECLIPSEtoAggREMINDsectors.csv"), stringsAsFactors=TRUE)
  map_sectors_Agg2REMIND     <- read.csv(toolMappingFile("sectoral", "mappingAggREMINDtoREMINDsectors.csv"), stringsAsFactors=TRUE)
  map_sectors_ECLIPSE2REMIND <- read.csv(toolMappingFile("sectoral", "mappingECLIPSEtoREMINDsectors.csv"), stringsAsFactors=TRUE)
  #map_sectors <- map_sectors[which(!is.na(map_sectors$EDGE)),] # Remove transport sector (which is not represented in EDGE)
  
  # read in regional map (select ISO and GAINS codes only). This is required for the construction of the SSPs
  map_REMINDregions  <- read.csv2(toolMappingFile("regional", "regionmappingREMIND.csv"), stringsAsFactors=TRUE)
  map_regions  <- read.csv2(toolMappingFile("regional", p_dagg_map), stringsAsFactors=TRUE)[,c(2,3)] 
  map_regions  <- map_regions %>%  
    filter_(~CountryCode != "ANT") %>% # Remove Netherland Antilles (not in REMIND regional mapping)
    filter_(~RegionCode != "") %>% 
    mutate_(RegionCode = ~gsub("\\ \\+", "\\+", gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", RegionCode)))) %>% 
    mutate_(CountryCode = ~factor(CountryCode))

  #-- PROCESS DATA ------------------
  vcat(2,">> Process data... \n")
  # set of sectors for which emission factors are computed 
  dimSector_EF <- getNames(activities)[!getNames(activities) %in% c(dimSector_skipEF, dimSector_skipEF_edge)]
  
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
  ef_eclipse  <- emissions[,,dimSector_EF]  /
    activities[,,dimSector_EF] * 
    conv_kt_per_PJ_to_Tg_per_TWa
  getSets(ef_eclipse)             <- c("region", "year", "data1", "data2", "data3")
  
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
  
  # use same set names for both data
  getSets(ef_eclipse) <- setsEmissions
  # add dimension to identify data-type
  ef_eclipse <- add_dimension(ef_eclipse,add="type",nm="ef_eclipse")
  emissions  <- add_dimension(emissions,add="type",nm="emissions")
  
  x <- mbind(ef_eclipse,emissions)


  return(list(x           = x,
              weight      = NULL,
              unit        = "unit",
              description = "calcECLIPSE substitute"
              ))
}
