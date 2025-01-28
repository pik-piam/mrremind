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
  x <- readSource("IEA_WEO", subtype = "Invest_Costs")
  
  x[, , ] <- as.numeric(x[, , ]) # convertng data values into numeric
  
  # Various mapping files used to get needed mappings, for e.g., South Asia
  countries <- toolGetMapping("regionmappingREMIND.csv", where = "mappingfolder", type = "regional")
  countries2 <- toolGetMapping("regionmappingMAgPIE.csv", where = "mappingfolder", type = "regional")
  countries3 <- toolGetMapping("regionmappingSSP.csv", where = "mappingfolder", type = "regional")
  
  # For countries in Africa and South Asia (except India) use subcritical plant investment costs as coal plant costs
  x[c(
    countries$CountryCode[countries$RegionCode == "AFR"],
    countries2$CountryCode[countries2$RegionCode == "SAS"][-4]
  ), , c(
    "Coal.Steam Coal - SUPERCRITICAL",
    "Coal.Steam Coal - ULTRASUPERCRITICAL"
  )] <- 0
  
  # For countries in LAM, IND, FSU and MEA use supercritical plant investment costs as standard coal plant costs
  x[c(
    countries$CountryCode[countries$RegionCode == "LAM"], "IND",
    countries2$CountryCode[countries2$RegionCode == "FSU"],
    countries$CountryCode[countries$RegionCode == "MEA"]
  ), , c(
    "Coal.Steam Coal - SUBCRITICAL",
    "Coal.Steam Coal - ULTRASUPERCRITICAL"
  )] <- 0
  
  # For countries in OECD and CHN use ultrasupercritical investment costs
  x[c("CHN", "KOR", "MAC", "HKG", countries3$CountryCode[countries3$RegionCode == "OECD"]), , c(
    "Coal.Steam Coal - SUBCRITICAL",
    "Coal.Steam Coal - SUPERCRITICAL"
  )] <- 0
  
  # For remaining countries not covered above, use subcritical plant investment costs
  years <- getYears(x)
  for (y in years) {
    x[
      getRegions(x)[x[, y, "Coal.Steam Coal - SUBCRITICAL"] != 0 &
                      x[, y, "Coal.Steam Coal - SUPERCRITICAL"] != 0 &
                      x[, y, "Coal.Steam Coal - ULTRASUPERCRITICAL"] != 0], y,
      c(
        "Coal.Steam Coal - SUPERCRITICAL",
        "Coal.Steam Coal - ULTRASUPERCRITICAL"
      )
    ] <- 0
  }
  tech_mapping <- toolGetMapping("techmappingIeaWeoPgAssumptions.csv", where = "mrremind", type = "sectoral") %>%
    filter(!is.na(!!sym("tech")))
  
  # create new magpie object with names of corresponding REMIND technologies
  x_new <- new.magpie(getRegions(x), names = unique(tech_mapping$tech), years = getYears(x), fill = 0)
  
  # for "pc" add all types of coal plants so each country has one value of "pc"
  x_new[, , "pc"] <- x[, , "Coal.Steam Coal - SUBCRITICAL"] + x[, , "Coal.Steam Coal - SUPERCRITICAL"] +
    x[, , "Coal.Steam Coal - ULTRASUPERCRITICAL"]
  
  # for "spv" take 1/4 of costs from small-scale and 3/4 costs from large scale (utility scale)ildings
  x_new[, , "spv"] <- 0.75 * x[, , "Renewables.Solar photovoltaics - Large scale"] + 0.25 * x[, , "Renewables.Solar photovoltaics - Buildings"]
  
  x_new[, , "hydro"] <- x[, , "Renewables.Hydropower - large-scale"]
  
  # and Biomass CHP
  x_new[, , "biochp"] <- 0.75 * x[, , "Renewables.Biomass CHP Medium"] + 0.25 * x[, , "Renewables.Biomass CHP Small"]
  
  # for rest of technologies, simply match
  further_techs_to_map <- dplyr::filter(tech_mapping, !(!!sym("tech") %in% c("pc", "spv", "hydro", "biochp")))
  x_new[, , further_techs_to_map$tech] <- as.numeric(x[, , further_techs_to_map$IEA])
  
  x_new <- time_interpolate(x_new, c(2025, 2035), integrate_interpolated_years = TRUE)
  
  # overwrite investment costs for renewables with data form REN21
  # did not find any explicit info, but REN costs should be roughly in US dollars (2015 values)
  x_REN21 <- readSource("REN21", subtype = "investmentCosts")
  x_REN21_wa <- collapseNames(x_REN21[, , "wa"]) # use weighted average
  getNames(x_REN21_wa) <- gsub("hydropower", "hydro", getNames(x_REN21_wa))
  getNames(x_REN21_wa) <- gsub("Solar PV", "spv", getNames(x_REN21_wa))
  getNames(x_REN21_wa) <- gsub("wind-on", "wind", getNames(x_REN21_wa))
  x_REN21_wa <- x_REN21_wa[, , c("Biopower", "Geothermal Power", "wind-off", "csp"), invert = TRUE]
  
  # use REN21 data only for 2015 for spv,csp and wind; all other time steps are 0
  x_new[, , c("spv", "wind")] <- 0
  x_new[, 2015, c("spv", "wind")] <- x_REN21_wa[, , c("spv", "wind")]
  x_new["JPN", 2015, "wind"] <- x_REN21["JPN", , "wind-on.max"]
  x_new["JPN", 2015, "spv"] <- 2000 # in USD/KW, source attached in input folder IEA_WEO
  # (National_Survey_Report_of_PV_Power_Applications_in_Japan_-_2017.pdf)
  x_new["JPN", 2015, "hydro"] <- 2400 # in USD/KW, source is the 2016 WEO numbers - they seem more reliable here than the Oceania data of <2000USD/kW
  # as Japan is not substantially expanding hydro even at high electricity prices
  
  ### Australia ###
  
  # for wind 2015: take average of Europe and USA from REN21 (not from "Oceania" as before)
  x_new["AUS", 2015, c("wind")] <- setNames(dimSums(x_REN21[c("USA", "FRA"), , c("wind-on.wa")], dim = 1) / 2, c("wind"))
  # for solar pv 2015: take IRENA number for large-scale solar investment cost by 2016
  # (neglect that rooftop is a bit more expensive)
  # source: https://www.irena.org/-/media/Files/IRENA/Agency/Publication/2018/Jan/IRENA_2017_Power_Costs_2018.pdf
  x_new["AUS", 2015, "spv"] <- 1400 # in USD/kW
  
  ### RP/FS: add PV investment cost for 2020
  # based on IEA PVPS data from 2018,
  # some regions manually adjusted
  
  # add some manual adjustments to IEA PVPS data for 2020 PV investment cost input data
  regmapping <- toolGetMapping("regionmappingH12.csv", where = "mappingfolder", type = "regional")
  x_adj <- new.magpie(unique(regmapping$RegionCode), years = "y2020", fill = NA)
  
  # CAZ: The Australian utility-scale market saw a stong jump upwards in 2018.
  # It is likely that the reported prices are a result of this jump, as in contrast,
  # rooftop is already well-established and has prices around 1.25$/W.
  # Accordingly, we expect that by 2020, the utility-scale solar market will be more
  # in equilibrium and have prices below the current rooftop prices
  # Also, the 2023 Australia PVPS report states ~1.5AUSD/W for utility-scale for 2018-2022,
  # equivalent to 1.05USD2020/W, and the Canadian PVPS report has 1.3CanD/W = 1USD/W
  x_adj["CAZ", , ] <- 1000
  # The Chinese PVPS report states ~3.5RMB/W, ~0.5$/W for utility-scale, in 2022
  x_adj["CHA", , ] <- 600
  # IND: Other sources for Indian utility-scale prices (IRENA, WEO, REN21) are more in the range of ~800$/kW.
  # Also, the reports around failed auctions and non-delivery of projects might indicate that
  # the stated prices are below cost.
  x_adj["IND", , ] <- 700
  # JPN: We assume that Japan prices will be downward-influenced by the low prices realized everywhere else in Asia. Also, the 2022 PVPS
  # report for JPN shows ~120-130 JPY/W, roughly 1.1US$2020/W
  x_adj["JPN", , ] <- 1100
  # LAM: IRENA states utility-scale prices in the range of ~1400-1500$/kW for LAM.
  # Recent auctions in individual countries indicate lower prices, but it is unlikely that
  # all the countries will immediately achieve the lowest prices realized in some auctions
  x_adj["LAM", , ] <- 850
  # MEA: IRENA states utility-scale prices in the range of ~1250$/kW for MEA
  # Recent auctions in individual countries indicate lower prices,
  # but it is unlikely that all the countries will immediately achieve
  # the lowest prices realized in some auctions
  x_adj["MEA", , ] <- 800
  # REF: Very different costs in IRENA (2300) and REN21 (1300) -
  # but unlikely to have higher capital costs than severely space-constrained Japan
  x_adj["REF", , ] <- 1100
  # SSA: IRENA states prices of ~1600$/W, but very likely that prices are currently decreasing through learning
  # from the low prices in North African countries.
  x_adj["SSA", , ] <- 1100
  
  # disaggregate adjustments to iso level
  x_adj_iso <- toolAggregate(x_adj, regmapping)
  
  # regions which have not been manually adjusted -> replace by original IEA PVPS data
  # IEAP PVPS are expressed in US dollars (2015 values)
  x_IEA_PVPS <- readSource("IEA_PVPS")
  
  x_adj_iso[which(is.na(x_adj_iso))] <- x_IEA_PVPS[which(is.na(x_adj_iso))]
  
  # add new 2020 values PV
  x_new[, "y2020", "spv"] <- x_adj_iso
  
  # convert data from $2015 to $2017
  x_new <- GDPuc::toolConvertGDP(
    gdp = x_new,
    unit_in = "constant 2015 US$MER",
    unit_out = mrdrivers::toolGetUnitDollar(),
    replace_NAs = "with_USA"
  )
  
  return(list(
    x = x_new,
    weight = x_new,
    unit = "US$2017/KW",
    description = "Investment costs data"
  ))

}
