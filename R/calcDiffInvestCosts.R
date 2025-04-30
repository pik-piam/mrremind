#' Aggregated investment cost data for REMIND regions (based on IEA_WEO)
#' @description Disaggregated investment cost data is aggregated and technologies renamed to REMIND names
#' @details REMIND does not have a classification of coal power plants e.g., sub-critical.
#' Therefore, countries are given coal plant costs assuming what type of coal plants are
#' expected to develop there. For other technologies, certain assumptions are taken
#' to change to REMIND convention.
#' @return Magpie object with aggregated but differentiated investment costs for some technologies.
#' @author Aman Malik

calcDiffInvestCosts <- function() {

  # IEA WEO 2016 costs are expressed in US dollars (2015 values)
  costsWEO <- readSource("IEA_WEO", subtype = "Invest_Costs")
  costsWEO[, , ] <- as.numeric(costsWEO[, , ]) # convert data values into numeric

  # Various mapping files used to get needed mappings, for e.g., South Asia
  regiRemind <- toolGetMapping("regionmappingREMIND.csv", where = "mappingfolder", type = "regional")
  regiMagpie <- toolGetMapping("regionmappingMAgPIE.csv", where = "mappingfolder", type = "regional")
  regiSSP <- toolGetMapping("regionmappingSSP.csv", where = "mappingfolder", type = "regional")

  # For countries in Africa and South Asia (except India) use subcritical plant investment costs as coal plant costs
  costsWEO[c(
    regiRemind$CountryCode[regiRemind$RegionCode == "AFR"],
    regiMagpie$CountryCode[regiMagpie$RegionCode == "SAS" & regiMagpie$CountryCode != "IND"]
  ), , c(
    "Coal.Steam Coal - SUPERCRITICAL",
    "Coal.Steam Coal - ULTRASUPERCRITICAL"
  )] <- 0

  # For countries in IND, LAM, MEA and FSU use supercritical plant investment costs as standard coal plant costs
  costsWEO[c("IND",
    regiRemind$CountryCode[regiRemind$RegionCode == "LAM"],
    regiRemind$CountryCode[regiRemind$RegionCode == "MEA"],
    regiMagpie$CountryCode[regiMagpie$RegionCode == "FSU"]
  ), , c(
    "Coal.Steam Coal - SUBCRITICAL",
    "Coal.Steam Coal - ULTRASUPERCRITICAL"
  )] <- 0

  # For countries in OECD and CHN use ultrasupercritical investment costs
  costsWEO[c("CHN", "KOR", "MAC", "HKG", regiSSP$CountryCode[regiSSP$RegionCode == "OECD"]), , c(
    "Coal.Steam Coal - SUBCRITICAL",
    "Coal.Steam Coal - SUPERCRITICAL"
  )] <- 0

  # For remaining countries not covered above, use subcritical plant investment costs
  years <- getYears(costsWEO)
  for (y in years) {
    costsWEO[
      getRegions(costsWEO)[costsWEO[, y, "Coal.Steam Coal - SUBCRITICAL"] != 0 &
                      costsWEO[, y, "Coal.Steam Coal - SUPERCRITICAL"] != 0 &
                      costsWEO[, y, "Coal.Steam Coal - ULTRASUPERCRITICAL"] != 0], y,
      c(
        "Coal.Steam Coal - SUPERCRITICAL",
        "Coal.Steam Coal - ULTRASUPERCRITICAL"
      )
    ] <- 0
  }

  # create new magpie object with names of corresponding REMIND technologies
  tech_mapping <- toolGetMapping("techmappingIeaWeoPgAssumptions.csv", where = "mrremind", type = "sectoral") %>%
    filter(!is.na(.data$tech))
  costsRemind <- new.magpie(getRegions(costsWEO), names = unique(tech_mapping$tech), years = getYears(costsWEO), fill = 0)

  # for "pc" add all types of coal plants so each country has one value of "pc"
  costsRemind[, , "pc"] <- costsWEO[, , "Coal.Steam Coal - SUBCRITICAL"] + costsWEO[, , "Coal.Steam Coal - SUPERCRITICAL"] +
    costsWEO[, , "Coal.Steam Coal - ULTRASUPERCRITICAL"]

  # for "spv" take 3/4 costs from large-scale (utility scale) and 1/4 costs from small-scale (buildings)
  costsRemind[, , "spv"] <- 0.75 * costsWEO[, , "Renewables.Solar photovoltaics - Large scale"] + 0.25 * costsWEO[, , "Renewables.Solar photovoltaics - Buildings"]

  # for "hydro" only consider large-scale
  costsRemind[, , "hydro"] <- costsWEO[, , "Renewables.Hydropower - large-scale"]

  # for biomass combined heat and power, take 3/4 costs from medium and 1/4 costs from small
  costsRemind[, , "biochp"] <- 0.75 * costsWEO[, , "Renewables.Biomass CHP Medium"] + 0.25 * costsWEO[, , "Renewables.Biomass CHP Small"]

  # for rest of technologies, simply match
  further_techs_to_map <- tech_mapping %>% filter(! .data$tech %in% c("pc", "spv", "hydro", "biochp"))
  costsRemind[, , further_techs_to_map$tech] <- as.numeric(costsWEO[, , further_techs_to_map$IEA])

  costsRemind <- time_interpolate(costsRemind, c(2025, 2035), integrate_interpolated_years = TRUE)

  # overwrite investment costs for renewables with data form REN21
  # did not find any explicit info, but REN costs should be roughly in US dollars (2015 values)
  costsREN21 <- readSource("REN21", subtype = "investmentCosts")
  costsREN21avg <- collapseNames(costsREN21[, , "wa"]) # use weighted average
  getNames(costsREN21avg) <- gsub("hydropower", "hydro", getNames(costsREN21avg))
  getNames(costsREN21avg) <- gsub("Solar PV", "spv", getNames(costsREN21avg))
  getNames(costsREN21avg) <- gsub("wind-on", "windon", getNames(costsREN21avg))
  costsREN21avg <- costsREN21avg[, , c("Biopower", "Geothermal Power", "csp", "wind-off"), invert = TRUE]

  # use REN21 data only for 2015 for spv and windon; all other time steps are 0
  costsRemind[, , c("spv", "windon")] <- 0
  costsRemind[, 2015, c("spv", "windon")] <- costsREN21avg[, , c("spv", "windon")]
  costsRemind["JPN", 2015, "windon"] <- costsREN21["JPN", , "wind-on.max"]
  costsRemind["JPN", 2015, "spv"] <- 2000 # in USD/KW, source attached in input folder IEA_WEO
  # (National_Survey_Report_of_PV_Power_Applications_in_Japan_-_2017.pdf)
  costsRemind["JPN", 2015, "hydro"] <- 2400 # in USD/KW, source is the 2016 WEO numbers - they seem more reliable here than the Oceania data of <2000USD/kW
  # as Japan is not substantially expanding hydro even at high electricity prices

  ### Australia ###

  # for wind 2015: take average of Europe and USA from REN21 (not from "Oceania" as before)
  costsRemind["AUS", 2015, "windon"] <- setNames(dimSums(costsREN21avg[c("USA", "FRA"), , "windon"], dim = 1) / 2, "windon")
  # for solar pv 2015: take IRENA number for large-scale solar investment cost by 2016
  # (neglect that rooftop is a bit more expensive)
  # source: https://www.irena.org/-/media/Files/IRENA/Agency/Publication/2018/Jan/IRENA_2017_Power_Costs_2018.pdf
  costsRemind["AUS", 2015, "spv"] <- 1400 # in USD/kW

  ### RP/FS: add PV investment cost for 2020
  # based on IEA PVPS data from 2018,
  # some regions manually adjusted

  # add some manual adjustments to IEA PVPS data for 2020 PV investment cost input data
  regmapping <- toolGetMapping("regionmappingH12.csv", where = "mappingfolder", type = "regional")
  costsManual <- new.magpie(unique(regmapping$RegionCode), years = "y2020", fill = NA)

  # CAZ: The Australian utility-scale market saw a stong jump upwards in 2018.
  # It is likely that the reported prices are a result of this jump, as in contrast,
  # rooftop is already well-established and has prices around 1.25$/W.
  # Accordingly, we expect that by 2020, the utility-scale solar market will be more
  # in equilibrium and have prices below the current rooftop prices
  # Also, the 2023 Australia PVPS report states ~1.5AUSD/W for utility-scale for 2018-2022,
  # equivalent to 1.05USD2020/W, and the Canadian PVPS report has 1.3CanD/W = 1USD/W
  costsManual["CAZ", , ] <- 1000
  # The Chinese PVPS report states ~3.5RMB/W, ~0.5$/W for utility-scale, in 2022
  costsManual["CHA", , ] <- 600
  # IND: Other sources for Indian utility-scale prices (IRENA, WEO, REN21) are more in the range of ~800$/kW.
  # Also, the reports around failed auctions and non-delivery of projects might indicate that
  # the stated prices are below cost.
  costsManual["IND", , ] <- 700
  # JPN: We assume that Japan prices will be downward-influenced by the low prices realized everywhere else in Asia. Also, the 2022 PVPS
  # report for JPN shows ~120-130 JPY/W, roughly 1.1US$2020/W
  costsManual["JPN", , ] <- 1100
  # LAM: IRENA states utility-scale prices in the range of ~1400-1500$/kW for LAM.
  # Recent auctions in individual countries indicate lower prices, but it is unlikely that
  # all the countries will immediately achieve the lowest prices realized in some auctions
  costsManual["LAM", , ] <- 850
  # MEA: IRENA states utility-scale prices in the range of ~1250$/kW for MEA
  # Recent auctions in individual countries indicate lower prices,
  # but it is unlikely that all the countries will immediately achieve
  # the lowest prices realized in some auctions
  costsManual["MEA", , ] <- 800
  # REF: Very different costs in IRENA (2300) and REN21 (1300) -
  # but unlikely to have higher capital costs than severely space-constrained Japan
  costsManual["REF", , ] <- 1100
  # SSA: IRENA states prices of ~1600$/W, but very likely that prices are currently decreasing through learning
  # from the low prices in North African countries.
  costsManual["SSA", , ] <- 1100

  # disaggregate adjustments to iso level
  costsManual <- toolAggregate(costsManual, regmapping)

  # regions which have not been manually adjusted -> replace by original IEA PVPS data
  # IEAP PVPS are expressed in US dollars (2015 values)
  costsPVPS <- readSource("IEA_PVPS")
  costsManual[which(is.na(costsManual))] <- costsPVPS[which(is.na(costsManual))]

  # add new 2020 values PV
  costsRemind[, "y2020", "spv"] <- costsManual

  # convert data from $2015 to $2017
  costsRemind <- GDPuc::toolConvertGDP(
    gdp = costsRemind,
    unit_in = "constant 2015 US$MER",
    unit_out = mrdrivers::toolGetUnitDollar(),
    replace_NAs = "with_USA"
  )

  weight <- costsRemind

  # avoid zero weights, as they cause a warning in aggregation
  weight[weight == 0] <- 1e-10

  return(list(
    x = costsRemind,
    weight = weight,
    unit = "US$2017/KW",
    description = "Investment costs data"
  ))

}
