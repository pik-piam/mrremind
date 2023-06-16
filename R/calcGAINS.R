#' calcGAINS
#'
#' Calculates air pollutant emissions and emission factors (user can choose)
#' based on GAINS emissions and activity data. Result is given on GAINS sector level.
#' User can choose between aggregated and extended sectoral resolution. Results are
#' given for multiple scenarios. Scenario design is partly taken from the GAINS data
#' and partly created in this function (particularly the SSPs).
#'
#' @param subtype decides whether emissions or emission factors are returned
#' @param sectoral_resolution aggreaged or extenden (uses different GAINS input data)
#' @importFrom dplyr group_by_ summarise_ ungroup mutate_ rename_ filter_ select_
#' @importFrom magclass as.magpie getCells getSets<- getNames<- getSets getRegions<- mselect<- setNames write.magpie
#' @importFrom tidyr gather_
#' @importFrom utils read.csv read.csv2
#' @importFrom quitte as.quitte



calcGAINS <- function(subtype = "emission_factors", sectoral_resolution = "extended") {

  if (!(subtype %in% c("emission_factors", "emissions"))) stop('subtype must be in c("emission_factors", "emissions")')

  # local functions

  # country to region
  allocate_c2r_ef <- function(id_ef, ip_region, ip_country, ip_year, ip_scenario) {
    dummy                   <- id_ef[ip_region, ip_year, ip_scenario]
    dummy[, , ]               <- setCells(id_ef[ip_country, ip_year, ip_scenario], "GLO")
    # names(dimnames(dummy))  <- c("region", "years", "data1.data2.species.scenario")
    return(dummy)
  }

  #
  allocate_min2r_ef <- function(id_ef, ip_region, ip_countryGroup, ip_year, ip_scenario) {
    dummy <- id_ef[ip_region, ip_year, ip_scenario]
    # Get minimum values across country group
    tmp <- as.quitte(id_ef[ip_countryGroup, ip_year, ip_scenario]) %>%
      group_by(!!!syms(c('data1', 'data2'))) %>%
      summarise(value = ifelse(all(.data$value == 0), 0,
                               min(.data$value[.data$value > 0], na.rm = TRUE))
                ) %>%  # a value 0 is often a sign for a NA that has been replaced with 0 for small countries
      ungroup() %>%
      as.data.frame() %>%
      as.quitte() %>%
      as.magpie()
    # Allocate minimum values to region
    dummy[ip_region, ip_year, ip_scenario] <- setYears(tmp)
    return(dummy)
  }
  # DK: deleted unused function

  # conversion factors
  conv_ktSO2_to_ktS            <- 1 / 2     # 32/(32+2*16)
  conv_kt_per_PJ_to_Tg_per_TWa <- 1e-3 / (1e15 / (365 * 24 * 60 * 60) * 1e-12)
  conv_PJ_to_Twa               <- (1e15 / (365 * 24 * 60 * 60) * 1e-12)

  # user-defined parameters
  time     <- c(seq(2005, 2055, 5), seq(2060, 2110, 10), 2130, 2150)
  scenario <- c("SSP1", "SSP2", "SSP5", "FLE", "MFR", "CLE") # ,"SSP3","SSP4", "MFR_Transports", "GlobalEURO6", "FLE_building_transport", "SLCF_building_transport")

  p_dagg_year <- 2005
  p_dagg_pop  <- "pop_SSP2"
  p_dagg_gdp  <- "gdp_SSP2"

  p_countryCategories <- "useGAINSregions" # "perCountry"

  # list of OECD countries
  # TODO: may want to place this in a mapping file or in a R library
  r_oecd <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA",
              "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

  # set of sectors for which no emission factor will be computed (because there is no activity reported, or not in terms of energy)
  dimSector_skipEF <- c("AACID", "CEMENT", "CHEM", "CHEMBULK", "CUSM", "NACID", "PAPER", "STEEL",
                       "Losses_Coal", "Losses_Distribution_Use",
                       "Transformations_Coal", "Transformations_HLF", "Transformations_HLF_Refinery", "Transformations_NatGas")

  dimSector_skipEF_edge <- c("End_Use_Industry_Bio_Trad", "End_Use_Industry_Coal", "End_Use_Industry_HLF", "End_Use_Industry_LLF",
                            "End_Use_Industry_NatGas", "End_Use_Residential_Bio_Mod", "End_Use_Residential_Bio_Trad", "End_Use_Residential_Coal",
                            "End_Use_Residential_HLF", "End_Use_Residential_LLF", "End_Use_Residential_NatGas", "End_Use_Services_Bio_Trad",
                            "End_Use_Services_Coal")

  dimSector_skipEF_edge <- c("")
  dimSector_skipEF <- c("")

  #-- READ IN ECLIPSE (GAINS) DATA ------------------
  activities <- readSource("ECLIPSE", subtype = paste0("activities.", sectoral_resolution))
  activities <- activities[, c(2005, 2010, 2020, 2030, 2050), ]

  emissions <- readSource("ECLIPSE", subtype = paste0("emissions.", sectoral_resolution))
  emissions <- emissions[, c(2005, 2010, 2020, 2030, 2050), ]

  # read in sectoral mapping (ECLIPSE (IMAGE) <> REMIND)
  # DK map_sectors_ECLIPSE2Agg    <- read.csv(toolGetMapping(type = "sectoral",
  #                                                          name = "mappingECLIPSEtoAggREMINDsectors.csv",
  #                                                          returnPathOnly = TRUE, where = "mappingfolder"),
  #                                           stringsAsFactors=TRUE)
  # DK map_sectors_Agg2REMIND     <- read.csv(toolGetMapping(type = "sectoral",
  #                                                          name = "mappingAggREMINDtoREMINDsectors.csv"
  #                                                          returnPathOnly = TRUE),
  #                                           stringsAsFactors=TRUE)
  # map_sectors <- map_sectors[which(!is.na(map_sectors$EDGE)),] # Remove transport sector (which is not represented in EDGE)

  # read in regional map (select ISO and GAINS codes only). This is required for the construction of the SSPs
  map_regions  <- read.csv2(toolGetMapping(type = "regional", name = "regionmappingGAINS.csv", returnPathOnly = TRUE, where = "mappingfolder"),
                            stringsAsFactors = TRUE)[, c(2, 3)]
  map_regions  <- map_regions %>%
    filter(.data$CountryCode != "ANT") %>% # Remove Netherland Antilles (not in REMIND regional mapping)
    filter(.data$RegionCode != "") %>%
    mutate(RegionCode = gsub("\\ \\+", "\\+",
                             gsub("^\\s+|\\s+$", "",
                                  gsub("[0-9]", "", .data$RegionCode)))) %>%
    mutate(CountryCode = factor(.data$CountryCode))

  # read in population and GDP data. required to compute gdp per cap
  pop <- calcOutput("Population", aggregate = FALSE)[, p_dagg_year, p_dagg_pop]
  gdp <- calcOutput("GDP",    aggregate = FALSE)[, p_dagg_year, p_dagg_gdp]

  # co <- map_regions$CountryCode[map_regions$RegionCode %in% c("Northern Africa","Middle East","Asia-Stan","Russia+")]
  # e <- dimSums(emissions["IRN",,"End_Use_Industry_Coal.VOC"],dim=1)
  # a <- dimSums(activities["IRN",,"End_Use_Industry_Coal"],dim=1)

  #-- PROCESS DATA ------------------
  # set of sectors for which emission factors are computed
  dimSector_EF <- getNames(activities)[!getNames(activities) %in% c(dimSector_skipEF, dimSector_skipEF_edge)]

  # calculate gdp per capita
  gdp_cap <- gdp / pop
  gdp_cap[is.na(gdp_cap)]   <- 0       # set NA to 0

  # Regional selections
  # select one country pertaining to WEU (all WEU countries should have the same EF). Used for SSP scenario rules
  select_weu <- paste(map_regions[which(map_regions$RegionCode == "Western Europe")[1], 1])

  # Retrieve Transport names
  # transportNames <- getNames(activities)[grepl("End_Use_Transport", getNames(activities))]

  # buildingNames  <- getNames(activities)[grepl("End_Use_Industry|End_Use_Residential|End_Use_Services", getNames(activities))]

  # convert SO2 emission from TgSO2 to TgS
  # emissions[,,"SO2"] <- emissions[,,"SO2"]*conv_ktSO2_to_ktS

  # define missing SLE scenario (assumed to be 3/4 of the distance between CLE and MFR, according to discussion with Zig Klimont on 18th Feb 2016)
  cle <- emissions[, , "CLE"]
  getNames(cle) <- gsub("CLE", "MFR", getNames(cle))
  sle <- cle - (cle - emissions[, , "MFR"]) * 0.75
  getNames(sle) <- gsub("MFR", "SLE", getNames(sle))
  emissions <- mbind(emissions, sle)
  rm(cle, sle)

  # calculate emission factors (only for power and end-use sectors, and not empty activities) and convert from kt/PJ to Tg/Twa
  ef_eclipse  <- emissions[, , dimSector_EF] / activities[, , dimSector_EF] * conv_kt_per_PJ_to_Tg_per_TWa

  getSets(ef_eclipse) <- c("region", "year", "data1", "data2", "data3")


  # DK: NAs in ef_eclipse: There are two potential reasons for NAs in ef_eclipse:
  # 1) ef = emi / activitiy => if the activity is zero the ef gets NA. The activity is disaggregated from 24 GAINS regions to the 249 ISO
  # countries using population as weight. If there is no population data for a country the activity (and also the emissions) of this country
  # are zero. In this case ef for this country is NA for ALL sectors and species => Jerome's command below that collects regions in NAregions
  # which have only NAs works! The NAs are replaced with ef from countries that belong to the same GAINS region and thus have identical ef.
  # After replacing these kind of NAs there may remain NAs for another reason:
  # 2) There is no activity data for a particular sector and GAINS region in the source data => activity will be zero for this sector and all
  # ISO countries belonging to this GAINS region => ef will be NA for those sectors and countries. Set those NAs to zero. When ef is reaggregated
  # to REMIND regions this is done using the activites as weight. And the activity is zero for the countries and sectors that have zeros due to zero
  # activity and thus have no effect on the result.
  #

  # some regions/countries have NA values everywhere (pop data is zero). Allocate EF of another country that belongs to the same GAINS region (except for Antartica)
  # Find countries that have NA for all sectors and species (then probably due to missing population data, see above)
  ef_eclipse["ATA", , ] <- 0    # Antartica -> 0
  NAregions <- c("AIA", "ALA", "ATF", "BES", "BLM", "BVT", "CCK", "COK", "CXR", "ESH", "FLK", "GGY", "GIB", "GLP", "GUF", "HMD", "IOT", "JEY", "MSR", "MTQ", "MYT", "NFK",
                 "NIU", "NRU", "PCN", "REU", "SGS", "SHN", "SJM", "SPM", "TKL", "TWN", "UMI", "VAT", "VGB", "WLF")
  # NAregions <- names(which(
  #                    sapply(
  #                      getRegions( which(is.na(ef_eclipse), arr.ind = TRUE)), function(k) all(is.na(as.numeric(ef_eclipse[k,,])))
  #                    )
  #              )
  # )


  # NAregions <- c("AIA", "ATF", "BVT", "CCK", "COK", "CXR", "ESH", "FLK", "GIB", "GLP", "GUF",
  #               "HMD", "IOT", "MSR", "MTQ", "MYT", "NFK", "NIU", "NRU", "PCN",
  #               "REU", "SGS", "SHN", "SJM", "SPM", "TKL", "TWN", "UMI", "VAT", "VGB", "WLF")
  MissingRegions <- c("ALA", "BES", "BLM", "CUW", "GGY", "IMN", "JEY", "MAF", "PSE", "SSD", "SXM")
  # AssociatedGAINSregions <- c("Western Europe", "Rest Central America", "Rest Central America", "Rest Central America", "Western Europe", "Western Europe", "Western Europe",
  #                            "Rest Central America", "Middle East", "Northern Africa", "Rest Central America")


  # for countries that have NAs everywhere (missing population, see reason 1 above) replace NA with ef of country that belongs to same GAINS region
  cat("ef_eclipse: NA values in first country replaced with ef of second country:\n")
  for (kregi in NAregions) {
    # suche die countrycodes aller countries, die zu der region gehören, zu der auch kregi gehört, lasse die Regionen aus NAregions und missingRegions weg
    # take the value of the first country since all countries that belong to the same GAINS region have the same ef
    subsitute_region <- map_regions$CountryCode[map_regions$RegionCode == map_regions$RegionCode[map_regions$CountryCode == kregi] &
                                              !map_regions$CountryCode %in% c(NAregions, MissingRegions)][1]
    cat(kregi, as.character(subsitute_region), "\n")
    # subsitute_region<-subsitute_region[-which(as.character(subsitute_region)=="ANT")] # remove ANT

    # if (all(is.na(ef_eclipse[subsitute_region,,]))) {
    #   # NAs in all countries
    # } elese {    }

    tmp <- ef_eclipse[subsitute_region, , ]
    getRegions(tmp) <- kregi
    ef_eclipse[kregi, , ] <- tmp
  }
  # some regions have no population data when disaggregating.
  # DK: I think this case was already addressed above (country has NAs everywhere)
  # for (kregi in MissingRegions) {
  #   # warum hier so kompliziert mit AssociatedGAINSregions und nicht automatisch wie oben?
  #   # es gibt einen Unterschied: oben würde SSD Western Africa zugeordnet, hier wird es Northern Africa zugeordnet
  #   substitute_region = map_regions$CountryCode[map_regions$RegionCode == AssociatedGAINSregions[which(MissingRegions == kregi)] &
  #                                              !map_regions$CountryCode %in% MissingRegions][1]
  #   tmp <- ef_eclipse[substitute_region,,]
  #   getRegions(tmp) <- kregi
  #   ef_eclipse[kregi,,] <- tmp
  # }

  # for the remaining NAs (0/0 = NaN) and Infs (1/0 = Inf ) just set EF to 0 (activity levels are 0, although in some cases emissions exist)
  ef_eclipse[is.na(ef_eclipse)]   <- 0
  ef_eclipse[is.infinite(ef_eclipse)]   <- 0
  rm(NAregions, MissingRegions) # , AssociatedGAINSregions)

  # DK: deleted outcommented code
  # Check ef corresponds to initial data

  # define exogenous emission data
  emissions_exogenous <- emissions # [,,dimSector_skipEF]

  # make output dummy "ef" and "emi" which then has to be filled by the data
  ef <- do.call("mbind",
                lapply(scenario,
                       function(s) {
new.magpie(getRegions(ef_eclipse),
                                               c(2005, 2010, 2030, 2050, 2100),
                                               gsub("CLE", s, getNames(ef_eclipse[, , "CLE"])))
                       }))

  emi <- do.call("mbind",
                lapply(scenario,
                       function(s) {
new.magpie(getRegions(emissions_exogenous),
                                               c(2005, 2010, 2030, 2050, 2100),
                                               gsub("CLE", s, getNames(emissions_exogenous[, , "CLE"])))
                       }))

  # define country categories
  if (p_countryCategories == "perCountry") {
    # low income countries (using World Bank definition < 2750 US$(2010)/Cap)
    r_L        <- dimnames(gdp_cap[getRegions(ef), , ])$ISO3[which(gdp_cap[getRegions(ef), , ] <= 2750)]
    # high and medium income countries
    r_HM       <- setdiff(getRegions(ef), r_L)
    # High-Medium income countries with strong pollution policies in place
    r_HMStrong <- c("AUS", "CAN", "USA", "JPN")                       # FIXME which definition???
    # High-Medium income countries with lower emissions goals
    r_HMRest   <- setdiff(r_HM, r_HMStrong)
  } else if (p_countryCategories == "useGAINSregions") {
    # Compute mean GDP/Cap per GAINS region
    regionMean_gdppcap <- sapply(unique(map_regions$RegionCode), function(x) {
mean(gdp_cap[map_regions$CountryCode[map_regions$RegionCode == x], , ])
})

    # low income countries (using World Bank definition < 2750 US$(2010)/Cap)
    r_L        <- levels(map_regions$CountryCode[map_regions$RegionCode %in% names(regionMean_gdppcap[regionMean_gdppcap <= 2750])])
    # high and medium income countries
    r_HM       <- setdiff(getRegions(ef), r_L)
    # High-Medium income countries with strong pollution policies in place
    r_HMStrong <- map_regions$CountryCode[map_regions$RegionCode %in% c("Western Europe", "Japan")]   # FIXME definition taken from JeS matlab script
    # High-Medium income countries with lower emissions goals
    r_HMRest   <- setdiff(r_HM, r_HMStrong)
  } else {
    stop("Unknown value of p_countryCategories")
  }

  # generate FLE and SSP scenarios
  # -------- Fix all scenarios to CLE in 2005 and 2010 ----------
  ef[, c(2005, 2010), ]  <- ef_eclipse[, c(2005, 2010), "CLE"]
  emi[, c(2005, 2010), ] <- emissions_exogenous[, c(2005, 2010), "CLE"]

  # ---------------- FLE ----------------------------------------
  # FLE: CLE 2010 emission factors and emissions are held constant
  ef[, , "FLE"]  <- setYears(ef[, 2010, "FLE"], NULL)     # NULL is actually the default value, skipping afterwards
  emi[, , "FLE"] <- setYears(emi[, 2010, "FLE"], NULL)

  # ---------------- SSP1 ---------------------------------------
  # Emission factors
  # low income countries
  ef[r_L, 2030, "SSP1"]   <- ef_eclipse[r_L, 2030, "CLE"]                                                          # 2030: CLE30
  ef[r_L, 2050, "SSP1"]   <- pmin(setYears(ef[r_L, 2030, "SSP1"]),
                                setYears(allocate_c2r_ef(ef_eclipse, r_L, select_weu, 2030, "CLE")))             # 2050: CLE30 WEU, if not higher than 2030 value
  ef[r_L, 2100, "SSP1"]   <- pmin(setYears(ef[r_L, 2050, "SSP1"]), setYears(ef_eclipse[r_L, 2030, "MFR"]))         # 2100: SLE30, if not higher than 2050 value
  # high income countries
  ef[r_HM, 2030, "SSP1"]  <- 0.75 * ef_eclipse[r_HM, 2030, "CLE"]                                                  # 2030: 75% of CLE30
  ef[r_HM, 2050, "SSP1"]  <- pmin(setYears(ef[r_HM,  2030, "SSP1"]), setYears(ef_eclipse[r_HM, 2030, "SLE"]))      # 2050: SLE30, if not higher than 2030 value
  ef[r_HM, 2100, "SSP1"]  <- pmin(setYears(ef[r_HM,  2050, "SSP1"]), setYears(ef_eclipse[r_HM, 2030, "MFR"]))      # 2100: MFR, if not higher than 2050 value

  # Emissions
  # low income countries
  emi[r_L, 2030, "SSP1"]   <- emissions_exogenous[r_L, 2030, "CLE"]                                                           # 2030: CLE30
  emi[r_L, 2050, "SSP1"]   <- pmin(setYears(emi[r_L, 2030, "SSP1"]), setYears(0.5 * emissions_exogenous[r_L, 2030, "CLE"]
                                                                          + 0.5 * emissions_exogenous[r_L, 2030, "SLE"]))     # 2050: CLE30 WEU, if not higher than 2030 value
  emi[r_L, 2100, "SSP1"]   <- pmin(setYears(emi[r_L, 2050, "SSP1"]), setYears(emissions_exogenous[r_L, 2030, "MFR"]))         # 2100: SLE30, if not higher than 2050 value
  # high income countries
  emi[r_HM, 2030, "SSP1"]  <- 0.75 * emissions_exogenous[r_HM, 2030, "CLE"]                                                   # 2030: 75% of CLE30
  emi[r_HM, 2050, "SSP1"]  <- pmin(setYears(emi[r_HM,  2030, "SSP1"]), setYears(emissions_exogenous[r_HM, 2030, "SLE"]))      # 2050: SLE30, if not higher than 2030 value
  emi[r_HM, 2100, "SSP1"]  <- pmin(setYears(emi[r_HM,  2050, "SSP1"]), setYears(emissions_exogenous[r_HM, 2030, "MFR"]))      # 2100: MFR, if not higher than 2050 value

  # ----------------- SSP2 --------------------------------------
  # Emission factors
  # High-Medium income countries with strong pollution policies in place
  ef[r_HMStrong, 2030, "SSP2"] <- ef_eclipse[r_HMStrong, 2030, "CLE"]                                                # 2030: CLE30
  ef[r_HMStrong, 2050, "SSP2"] <- pmin(setYears(ef[r_HMStrong,        2030, "SSP2"]),
                                     setYears(ef_eclipse[r_HMStrong, 2030, "SLE"]))                                # 2050: SLE30
  ef[r_HMStrong, 2100, "SSP2"] <- pmin(setYears(ef[r_HMStrong,        2050, "SSP2"]),
                                     setYears(allocate_min2r_ef(ef_eclipse, r_HMStrong, r_oecd, 2030, "SLE")))   # 2100: Lowest SLE30 or lower
  # High-Medium income countries with lower emissions goals
  ef[r_HMRest, 2030, "SSP2"]  <- ef_eclipse[r_HMRest, 2030, "CLE"]                                                   # 2030: CLE30
  ef[r_HMRest, 2050, "SSP2"]  <- pmin(setYears(ef[r_HMRest,       2030, "SSP2"]),
                                    setYears(allocate_min2r_ef(ef_eclipse, r_HMRest, r_HMRest, 2030, "CLE")))    # 2050: Min CLE30
  ef[r_HMRest, 2100, "SSP2"]  <- pmin(setYears(ef[r_HMRest, 2050, "SSP2"]),
                                    setYears(allocate_c2r_ef(ef_eclipse, r_HMRest, select_weu, 2030, "SLE")))    # 2100: SLE30 WEU
  # low income countries
  ef[r_L, 2030, "SSP2"]       <- setYears(ef_eclipse[r_L, 2020, "CLE"])                                            # 2030: CLE20
  ef[r_L, 2050, "SSP2"]       <- pmin(setYears(ef[r_L,       2030, "SSP2"]),
                                    setYears(allocate_min2r_ef(ef_eclipse, r_L, r_L, 2030, "CLE")))              # 2050: Min CLE30
  ef[r_L, 2100, "SSP2"]       <- pmin(setYears(ef[r_L, 2050, "SSP2"]),
                                    setYears(allocate_c2r_ef(ef_eclipse, r_L, select_weu, 2030, "CLE")))         # 2100: CLE30 WEU

  # Emissions
  # High-Medium income countries with strong pollution policies in place
  emi[r_HMStrong, 2030, "SSP2"] <- emissions_exogenous[r_HMStrong, 2030, "CLE"]                                               # 2030: CLE30
  emi[r_HMStrong, 2050, "SSP2"] <- pmin(setYears(emi[r_HMStrong,        2030, "SSP2"]),
                                     setYears(emissions_exogenous[r_HMStrong, 2030, "SLE"]))                                # 2050: SLE30
  emi[r_HMStrong, 2100, "SSP2"] <- pmin(setYears(emi[r_HMStrong,        2050, "SSP2"]),
                                      setYears(emissions_exogenous[r_HMStrong, 2030, "SLE"] * 0.8))                           # 2100: Lowest SLE30 or lower -> 0.8*SLE30
  # High-Medium income countries with lower emissions goals
  emi[r_HMRest, 2030, "SSP2"]  <- emissions_exogenous[r_HMRest, 2030, "CLE"]                                                  # 2030: CLE30
  emi[r_HMRest, 2050, "SSP2"]  <- pmin(setYears(emi[r_HMRest,       2030, "SSP2"]),
                                     setYears(emissions_exogenous[r_HMRest, 2030, "SLE"]))                                  # 2050: Min CLE30 -> SLE30
  emi[r_HMRest, 2100, "SSP2"]  <- pmin(setYears(emi[r_HMRest, 2050, "SSP2"]),
                                     setYears(emissions_exogenous[r_HMRest, 2030, "SLE"] * 0.8))                              # 2100: SLE30 WEU -> 0.8*SLE30
  # low income countries
  emi[r_L, 2030, "SSP2"]       <- setYears(emissions_exogenous[r_L, 2020, "CLE"])                                           # 2030: CLE20
  emi[r_L, 2050, "SSP2"]       <- pmin(setYears(emi[r_L, 2030, "SSP2"]),
                                     setYears(emissions_exogenous[r_L, 2030, "CLE"]))                                     # 2050: Min CLE30 -> CLE30
  emi[r_L, 2100, "SSP2"]       <- pmin(setYears(emi[r_L, 2050, "SSP2"]),
                                     setYears(emissions_exogenous[r_L, 2030, "SLE"] * 0.95))                                # 2100: CLE30 WEU -> 0.95*SLE30
  # DK: deleted outcommented code

  # -----------------SSP1<SSP2-----------------------------------

  ef[, 2030, "SSP1"]   <- pmin(setYears(ef[, 2030, "SSP2"]), setYears(ef[, 2030, "SSP1"]))
  ef[, 2050, "SSP1"]   <- pmin(setYears(ef[, 2050, "SSP2"]), setYears(ef[, 2050, "SSP1"]))  # make sure SSP1 is not higher than SSP2

  # ----------------- SSP5 --------------------------------------
  # set SSP5 to the values of SSP1
  ef[, , "SSP5"]  <- ef[, , "SSP1"]
  emi[, , "SSP5"] <- emi[, , "SSP1"] # does not really make sense...

  # DK: deleted outcommented code:

  # ----------------- CLE and MFR -------------------------------
  ef[, c(2005, 2010, 2030, 2050), c("CLE", "MFR")] <- ef_eclipse[, c(2005, 2010, 2030, 2050), c("CLE", "MFR")]
  ef[, 2100, c("CLE", "MFR")] <- setYears(ef_eclipse[, 2050, c("CLE", "MFR")])                           # for 2100, take the same values as in 2050

  emi[, c(2005, 2010, 2030, 2050), c("CLE", "MFR")] <- emissions_exogenous[, c(2005, 2010, 2030, 2050), c("CLE", "MFR")]
  emi[, 2100, c("CLE", "MFR")] <- setYears(emissions_exogenous[, 2050, c("CLE", "MFR")])                           # for 2100, take the same values as in 2050

  # DK: deleted outcommented code:
  # DK select the scenario before returning

  if (subtype == "emissions") {
    result <- time_interpolate(emi, interpolated_year = time, integrate_interpolated_years = TRUE, extrapolation_type = "constant")
    result <- result / 1000 # kt -> Mt
    getSets(result) <- c("region", "year", "sector", "emi", "scenario")
    w <- NULL
  } else if (subtype == "emission_factors") {
    common_y <- intersect(getYears(ef), getYears(activities))
    result <- time_interpolate(ef[, common_y, ],  interpolated_year = time, integrate_interpolated_years = TRUE, extrapolation_type = "constant")
    getSets(result) <- c("region", "year", "sector", "emi", "scenario")
    #
    w      <- time_interpolate(activities[, common_y, getNames(result, dim = 1)], interpolated_year = time, integrate_interpolated_years = TRUE, extrapolation_type = "constant")
    getSets(w) <- c("region", "year", "sector")
  } else {
 stop("Unknown subtype ", subtype, "!")
}

  return(list(x           = result,
              weight      = w,
              unit        = "Mt",
              description = "Scenario for emissions or emission factors calculated based on ECLIPSE (GAINS) data."))
}
