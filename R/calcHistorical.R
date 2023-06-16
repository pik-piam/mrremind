#' Gather reference data from various sources.
#' @importFrom magclass setNames getNames getSets add_columns
#' @importFrom dplyr filter group_by mutate select ungroup
#' @importFrom rlang syms
#' @importFrom tidyr complete nesting
calcHistorical <- function() {
  .fillZeros <- function(data) {
    Non28EUcountries <- c("ALA", "FRO", "GIB", "GGY", "IMN", "JEY")
    tmp <- data[Non28EUcountries, , ]
    tmp[is.na(tmp)] <- 0
    data[Non28EUcountries, , ] <- tmp[Non28EUcountries, , ]
    return(data)
  }

  # Final Energy
  fe_iea <- calcOutput("FE", source = "IEA", aggregate = FALSE)
  fe_iea <- add_dimension(fe_iea, dim = 3.1, add = "model", nm = "IEA")

  fe_weo <- calcOutput("FE", source = "IEA_WEO", aggregate = F)
  fe_weo <- fe_weo[, , "Current Policies Scenario", pmatch = T]
  fe_weo <- collapseNames(fe_weo)
  fe_weo <- add_dimension(fe_weo, dim = 3.1, add = "model", nm = "IEA_WEO")

  # Final Energy - Heat Roadmap Europe
  fe_hre <- calcOutput("HRE", aggregate = FALSE)

  # Primary Energy
  pe_iea <- calcOutput("PE", subtype = "IEA", aggregate = FALSE)
  pe_iea <- add_dimension(pe_iea, dim = 3.1, add = "model", nm = "IEA")

  pe_weo <- calcOutput("PE", subtype = "IEA_WEO", aggregate = FALSE)
  pe_weo <- pe_weo[, , "Current Policies Scenario", pmatch = T]
  pe_weo <- collapseNames(pe_weo)
  pe_weo <- add_dimension(pe_weo, dim = 3.1, add = "model", nm = "IEA_WEO")

  # fossil trade
  trade <- calcOutput("Trade", aggregate = FALSE)
  trade <- add_dimension(trade, dim = 3.1, add = "model", nm = "IEA")

  # Population
  pop <- calcOutput("PopulationPast", aggregate = FALSE)
  unit <- strsplit(grep("unit", attributes(pop)$comment, value = TRUE), split = ": ")[[1]][[2]]
  getNames(pop) <- paste0("Population (", unit, ")")
  pop <- add_dimension(pop, dim = 3.1, add = "model", nm = "WDI")

  # GDP in ppp
  gdpp_James <- calcOutput("GDPPast", aggregate = FALSE) / 1000
  getNames(gdpp_James) <- paste0("GDP|PPP (billion US$2005/yr)")
  gdpp_James <- add_dimension(gdpp_James, dim = 3.1, add = "model", nm = "James_IHME")

  gdpp_WB <- calcOutput("GDPPast", GDPPast = "WB_USD05_PPP_pc", aggregate = FALSE) / 1000
  getNames(gdpp_WB) <- paste0("GDP|PPP (billion US$2005/yr)")
  gdpp_WB <- add_dimension(gdpp_WB, dim = 3.1, add = "model", nm = "James_WB")

  gdpp_IMF <- calcOutput("GDPPast", GDPPast = "IMF_USD05_PPP_pc", aggregate = FALSE) / 1000
  getNames(gdpp_IMF) <- paste0("GDP|PPP (billion US$2005/yr)")
  gdpp_IMF <- add_dimension(gdpp_IMF, dim = 3.1, add = "model", nm = "James_IMF")

  # Historical emissions from CEDS data base
  ceds <- calcOutput("Emissions", datasource = "CEDS2021", aggregate = FALSE)

  # Add GHG total (removed while Land-Use Change is not available)
  # ceds <- add_columns(ceds,"Emi|GHGtot (Mt CO2-equiv/yr)",dim=3.1)
  # ceds[,,"Emi|GHGtot (Mt CO2-equiv/yr)"] <- ceds[,,"Emi|CO2 (Mt CO2/yr)"] +
  #  ceds[,,"Emi|CH4 (Mt CH4/yr)"]*28 +
  #  ceds[,,"Emi|N2O (kt N2O/yr)"]/1000*265
  ceds <- add_dimension(ceds, dim = 3.1, add = "model", nm = "CEDS")

  # Historical emissions from EDGAR v5.0 and v6.0
  edgar6 <- calcOutput("Emissions", datasource = "EDGAR6", aggregate = FALSE)
  edgar6 <- add_dimension(edgar6, dim = 3.1, add = "model", nm = "EDGAR6")

  # Historical emissions from PRIMAPhist data base
  # select total
  primap <- readSource("PRIMAPhist", "hist")[, , "CAT0"]
  # select CO2 and total GHG and convert into Co2
  primap <- primap[, , c("co2_c", "kyotoghgar4_co2eq_c")] / 12 * 44
  getNames(primap) <- c("Emi|CO2 (Mt CO2/yr)", "Emi|GHG (Mt CO2eq/yr)")
  primap <- add_dimension(primap, dim = 3.1, add = "model", nm = "PRIMAPhist")

  # Historical emissions from CDIAC data base
  cdiac <- calcOutput("Emissions", datasource = "CDIAC", aggregate = FALSE)
  getNames(cdiac) <- gsub("Emissions", "Emi", getNames(cdiac))
  getNames(cdiac) <- gsub("Mt/yr", "Mt CO2/yr", getNames(cdiac))
  cdiac <- add_dimension(cdiac, dim = 3.1, add = "model", nm = "CDIAC")

  # Historical land use emissions (taken from "mrvalidation/R/fullVALIDATION.R")
  LU_EDGAR_LU <- calcOutput(type = "LandEmissions", datasource = "EDGAR_LU", aggregate = FALSE, try = TRUE)
  LU_CEDS <- calcOutput(type = "LandEmissions", datasource = "CEDS", aggregate = FALSE, try = TRUE)
  LU_FAO_EmisLUC <- calcOutput(type = "LandEmissions", datasource = "FAO_EmisLUC", aggregate = FALSE, try = TRUE)
  LU_FAO_EmisAg <- calcOutput(type = "LandEmissions", datasource = "FAO_EmisAg", aggregate = FALSE, try = TRUE)
  LU_PRIMAPhist <- calcOutput(type = "LandEmissions", datasource = "PRIMAPhist", aggregate = FALSE, try = TRUE)

  # remove scenario dimension (will be added below as also for remind variables)
  LU_EDGAR_LU <- collapseNames(LU_EDGAR_LU, collapsedim = 1)
  LU_CEDS <- collapseNames(LU_CEDS, collapsedim = 1)
  LU_FAO_EmisLUC <- collapseNames(LU_FAO_EmisLUC, collapsedim = 1)
  LU_FAO_EmisAg <- collapseNames(LU_FAO_EmisAg, collapsedim = 1)
  LU_PRIMAPhist <- collapseNames(LU_PRIMAPhist, collapsedim = 1)
  # LU_IPCC        <- collapseNames(LU_IPCC       , collapsedim=1)
  # LU_Nsurplus2   <- collapseNames(LU_Nsurplus2  , collapsedim=1)

  # give ceds emissions from calcValidEmissions (magpie) a name that is different from ceds emissions from calcEmissions (remind)
  getNames(LU_CEDS, dim = 1) <- "ceds_lu"

  # remove duplicates from LU_FAO_EmisAg
  LU_FAO_EmisAg <- LU_FAO_EmisAg[, , which(!duplicated(getNames(LU_FAO_EmisAg)))]


  # Capacities historical data ====

  # IRENA capacities - technologies: "csp", "geohdr", "hydro", "spv", "wind"
  IRENAcap <- readSource(type = "IRENA", subtype = "Capacity")[, , c("Concentrated solar power", "Geothermal", "Hydropower", "Solar photovoltaic", "Wind")] # Read IRENA renewables capacity data
  IRENAcap <- IRENAcap * 1E-03 # converting MW to GW
  mapping <- data.frame(
    IRENA_techs = c("Concentrated solar power", "Geothermal", "Hydropower", "Solar photovoltaic", "Wind"),
    REMIND_var = c("Cap|Electricity|Solar|CSP (GW)", "Cap|Electricity|Geothermal (GW)", "Cap|Electricity|Hydro (GW)", "Cap|Electricity|Solar|PV (GW)", "Cap|Electricity|Wind (GW)"), stringsAsFactors = FALSE
  )
  IRENAcap <- luscale::rename_dimnames(IRENAcap, dim = 3, query = mapping, from = "IRENA_techs", to = "REMIND_var") # renaming technologies to REMIND naming convention
  IRENAcap <- mbind(IRENAcap, setNames(IRENAcap[, , "Cap|Electricity|Solar|CSP (GW)"] + IRENAcap[, , "Cap|Electricity|Solar|PV (GW)"], "Cap|Electricity|Solar (GW)"))
  IRENAcap <- add_dimension(IRENAcap, dim = 3.1, add = "model", nm = "IRENA")

  # Ember electricity data ====
  Ember <- calcOutput("Ember", subtype = "all", aggregate = FALSE)
  Ember <- add_dimension(Ember, dim = 3.1, add = "model", nm = "Ember")

  # Region specific historical data ====
  # European Eurostat data
  eurostat <- calcOutput("EuropeanEnergyDatasheets", subtype = "EU27", aggregate = FALSE)
  eurostat <- add_dimension(eurostat, dim = 3.1, add = "model", nm = "Eurostat")

  # Emissions market data
  # emiMktES <- setNames(readSource("Eurostat_EffortSharing",subtype="emissions"),"Emi|GHG|ESR (Mt CO2-equiv/yr)") # Effort Sharing
  # emiMktETS <- setNames(dimSums(readSource("EEA_EuropeanEnvironmentAgency",subtype="ETS")[,seq(2005,2019), c("2_ Verified emissions.20-99 All stationary installations","3_ Estimate to reflect current ETS scope for allowances and emissions.20-99 All stationary installations")]),"Emi|GHG|ETS (Mt CO2-equiv/yr)") #ETS without aviation
  # # national aviation is not included in REMIND ETS yet
  # # aviation <- readSource("EEA_EuropeanEnvironmentAgency",subtype="ETS")[,seq(2005,2018),c("2_ Verified emissions.10 Aviation")]
  # #set all non EU values to NA (by doing this we are excluding from the ETS the non EU28 countries - Norway, Liechtenstein and Iceland - because REMIND is not including them in the ETS)
  # emiMktES[getRegions(emiMktES)[-which(getRegions(emiMktES) %in% EUcountries)],,] <- NA
  # emiMktES <- add_dimension(emiMktES, dim=3.1, add="model",nm="Eurostat")
  # ETScountries <- c(EUcountries,"GRL","ISL","LIE","NOR","SJM","CHE")
  # emiMktETS[getRegions(emiMktETS)[-which(getRegions(emiMktETS) %in% ETScountries)],,] <- NA
  # emiMktETS <- add_dimension(emiMktETS, dim=3.1, add="model",nm="EEA_historical")
  # # set remaining emissions to other market - it is missing lulucf (Land use, land-use change, and forestry)
  # totalGHG <- dimSums(eurostat[,,c("Emi|GHGtot (Mt CO2-equiv/yr)","Emi|GHG|Bunkers|International Aviation (Mt CO2-equiv/yr)","Emi|GHG|Bunkers|International Maritime Transport (Mt CO2-equiv/yr)")])
  # years <- Reduce(intersect, list(getYears(totalGHG),getYears(emiMktES[,,"Emi|GHG|ESR (Mt CO2-equiv/yr)"]),getYears(emiMktETS[,,"Emi|GHG|ETS (Mt CO2-equiv/yr)"])))
  # emiMktESOthers <- setNames(collapseNames(totalGHG[,years,] - emiMktES[,years,"Emi|GHG|ESR (Mt CO2-equiv/yr)"] - emiMktETS[,years,"Emi|GHG|ETS (Mt CO2-equiv/yr)"]),"Emi|GHG|other - Non ETS and ESR (Mt CO2-equiv/yr)")
  # emiMktESOthers <- add_dimension(emiMktESOthers, dim=3.1, add="model",nm="Eurostat")

  # EEA GHG Projections
  EEA_GHGProjections <- .fillZeros(calcOutput("EEAGHGProjections", aggregate = FALSE))

  # EEA GHG Sectoral Historical Data
  EEA_GHGSectoral <- .fillZeros(readSource("EEA_EuropeanEnvironmentAgency", subtype = "sectoral"))
  EEA_GHGSectoral <- add_dimension(EEA_GHGSectoral, dim = 3.1, add = "model", nm = "EEA_historical")

  EEA_GHGTotal <- .fillZeros(readSource("EEA_EuropeanEnvironmentAgency", subtype = "total"))
  EEA_GHGTotal <- add_dimension(EEA_GHGTotal, dim = 3.1, add = "model", nm = "EEA_historical")

  # EEA_GHGES <- .fillZeros(readSource("EEA_EuropeanEnvironmentAgency", subtype="ESR"))
  # EEA_GHGES <- add_dimension(EEA_GHGES, dim=3.1,add="model",nm="EEA_historical")

  # EU Reference Scenario
  EU_ReferenceScenario <- calcOutput("EU_ReferenceScenario", aggregate = F)

  # ARIADNE Reference Scenario
  ARIADNE_ReferenceScenarioGdp <- readSource("ARIADNE", subtype = "gdp")
  ARIADNE_ReferenceScenarioGdp <- add_dimension(ARIADNE_ReferenceScenarioGdp,
    dim = 3.1, add = "model", nm = "ARIADNE"
  )

  ARIADNE_ReferenceScenarioGdpCorona <- readSource("ARIADNE", subtype = "gdp_corona")
  ARIADNE_ReferenceScenarioGdpCorona <- add_dimension(ARIADNE_ReferenceScenarioGdpCorona,
    dim = 3.1, add = "model", nm = "ARIADNE - Corona"
  )

  ARIADNE_ReferenceScenarioPop <- readSource("ARIADNE", subtype = "population")
  ARIADNE_ReferenceScenarioPop <- add_dimension(ARIADNE_ReferenceScenarioPop,
    dim = 3.1, add = "model", nm = "ARIADNE"
  )

  IEA_EVOutlook <- calcOutput("IEA_EVOutlook", aggregate = F)

  # Calculate Emission Reference Values
  Emi_Reference <- .fillZeros(calcOutput("EmiReference", aggregate = FALSE))
  Emi_Reference <- add_dimension(Emi_Reference, dim = 3.1, add = "model", nm = "EEA")

  # Eurostat emissions
  EUcountries <- c("ALA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "GGY", "HUN", "IRL", "IMN", "ITA", "JEY", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
  eurostatEmi <- readSource(type = "Eurostat", subtype = "emissions")
  eurostatEmi[getRegions(eurostatEmi)[-which(getRegions(eurostatEmi) %in% EUcountries)], , ] <- NA
  # set values for EU countries with no values to 0
  noData <- getItems(eurostatEmi[EUcountries, , ], dim = 1)[dimSums(abs(eurostatEmi[EUcountries, , ]), dim = c(2, 3), na.rm = TRUE) == 0]
  eurostatEmi[noData, , ] <- 0

  # conversion factors between CO2eq and N2O / CH4 are derived by Eurostat webtool comparison
  emiEurostat <- mbind(
    setNames(eurostatEmi[, , "CH4_native.Total (excluding memo items)"], "Emi|CH4 (Mt CH4/yr)"),
    setNames(eurostatEmi[, , "N2O_native.Total (excluding memo items)"], "Emi|N2O (kt N2O/yr)") * 1000,
    setNames(eurostatEmi[, , "GHG.Land use, land use change, and forestry (LULUCF)"], "Emi|GHG|Land-Use Change (Mt CO2eq/yr)"),
    setNames(eurostatEmi[, , "CO2.Land use, land use change, and forestry (LULUCF)"], "Emi|CO2|Land-Use Change (Mt CO2/yr)"),
    setNames(eurostatEmi[, , "CH4_native.Land use, land use change, and forestry (LULUCF)"], "Emi|CH4|Land-Use Change (Mt CH4/yr)"),
    setNames(eurostatEmi[, , "N2O_native.Land use, land use change, and forestry (LULUCF)"], "Emi|N2O|Land-Use Change (kt N2O/yr)") * 1000
  )
  emiEurostat <- add_dimension(emiEurostat, dim = 3.1, add = "model", nm = "Eurostat")

  # INNOPATHS data
  INNOPATHS <- calcOutput("INNOPATHS", aggregate = F)
  INNOPATHS <- add_dimension(INNOPATHS, dim = 3.1, add = "model", nm = "INNOPATHS")

  # JRC IDEES data
  JRC_Industry <- calcOutput("JRC_IDEES", subtype = "Industry", aggregate = FALSE)
  JRC_Industry <- add_dimension(JRC_Industry, dim = 3.1, add = "model", nm = "JRC")

  JRC_Transport <- calcOutput("JRC_IDEES", subtype = "Transport", aggregate = FALSE)
  JRC_Transport <- add_dimension(JRC_Transport, dim = 3.1, add = "model", nm = "JRC")

  JRC_ResCom <- calcOutput("JRC_IDEES", subtype = "ResCom", aggregate = FALSE)
  JRC_ResCom <- add_dimension(JRC_ResCom, dim = 3.1, add = "model", nm = "JRC")

  # AGEB final energy data
  AGEB_Bal <- calcOutput("AGEB", subtype = "balances", aggregate = FALSE)
  AGEB_Bal <- add_dimension(AGEB_Bal, dim = 3.1, add = "model", nm = "AGEB")

  AGEB_SE <- calcOutput("AGEB", subtype = "electricity", aggregate = FALSE)
  AGEB_SE <- add_dimension(AGEB_SE, dim = 3.1, add = "model", nm = "AGEB")

  # UBA Emission data
  UBA_emi <- calcOutput("UBA", aggregate = FALSE)
  UBA_emi <- add_dimension(UBA_emi, dim = 3.1, add = "model", nm = "UBA")

  # UNFCCC emission data
  UNFCCC <- calcOutput("UNFCCC", aggregate = FALSE)
  # remove years before 1990 due to incomplete data
  UNFCCC <- UNFCCC[, seq(1986, 1989, 1), , invert = T]
  UNFCCC <- add_dimension(UNFCCC, dim = 3.1, add = "model", nm = "UNFCCC")

  # BP data
  BP <- calcOutput("BP", aggregate = FALSE)
  BP <- add_dimension(BP, dim = 3.1, add = "model", nm = "BP")

  # Cement Production ----
  USGS_cement <- readSource(
    type = "USGS", subtype = "cement",
    convert = FALSE
  ) %>%
    quitte::madrat_mule() %>%
    group_by(!!!syms(c("iso3c", "year"))) %>%
    filter(max(.data$reporting.year) == .data$reporting.year) %>%
    ungroup() %>%
    select(-"reporting.year") %>%
    # t/year * 1e-6 Mt/t = Mt/year
    mutate(
      value = .data$value * 1e-6,
      model = "USGS",
      variable = "Production|Industry|Cement (Mt/yr)"
    ) %>%
    select("iso3c", "year", "model", "variable", "value") %>%
    complete(
      iso3c = unname(getISOlist()),
      year = unique(.data$year),
      fill = list(
        model = "USGS",
        variable = "Production|Industry|Cement (Mt/yr)",
        value = 0
      )
    ) %>%
    as.magpie(spatial = 1, temporal = 2, tidy = TRUE)

  # Steel Production ----
  worldsteel <- readSource("worldsteel", convert = FALSE) %>%
    quitte::madrat_mule() %>%
    filter(
      .data$name %in% c(
        "Production in Oxygen-Blown Converters",
        "Production in Open Hearth Furnaces",
        "DRI Production",
        "Production in Electric Arc Furnaces"
      ),
      .data$iso3c %in% (toolGetMapping(
        name = getConfig("regionmapping"),
        type = "regional", where = "mappingfolder"
      ) %>%
        pull("CountryCode"))
    ) %>%
    # kt/year * 1e-3 Mt/kt = Mt/year
    mutate(value = .data$value * 1e-3) %>%
    pivot_wider(values_fill = 0) %>%
    mutate(
      `Production|Industry|Steel (Mt/yr)` = .data$`Production in Oxygen-Blown Converters`
        + .data$`Production in Open Hearth Furnaces`
        + .data$`Production in Electric Arc Furnaces`,
      `Production|Industry|Steel|Secondary (Mt/yr)` =
      # Secondary steel production is production from EAF that does not use
      # inputs from DRI.  If mostly DRI is used for EAF, the difference might
      # be negative (different mass bases due to e.g. carbon content), so
      # limit to zero.
        pmax(
          0,
          .data$`Production in Electric Arc Furnaces`
            - .data$`DRI Production`
        ),
      `Production|Industry|Steel|Primary (Mt/yr)` = (.data$`Production|Industry|Steel (Mt/yr)`
        - .data$`Production|Industry|Steel|Secondary (Mt/yr)`
      ),
      source = "Worldsteel"
    ) %>%
    select(
      "iso3c", "year", "source", "Production|Industry|Steel (Mt/yr)",
      "Production|Industry|Steel|Primary (Mt/yr)",
      "Production|Industry|Steel|Secondary (Mt/yr)"
    ) %>%
    pivot_longer(c(
      "Production|Industry|Steel (Mt/yr)",
      "Production|Industry|Steel|Primary (Mt/yr)",
      "Production|Industry|Steel|Secondary (Mt/yr)"
    )) %>%
    complete(nesting(!!!syms(c("year", "source", "name"))),
      iso3c = toolGetMapping(
        name = getConfig("regionmapping"),
        type = "regional", where = "mappingfolder"
      ) %>%
        pull("CountryCode"),
      fill = list(value = 0)
    ) %>%
    as.magpie(spatial = 4, temporal = 1, data = ncol(.data))

  # Steel Stock ----
  steelStock <- calcOutput("SteelStock", aggregate = FALSE)
  steelStock <- add_dimension(steelStock, dim = 3.1, add = "model", nm = "Mueller")

  # blow up to union of years ====
  # find all existing years (y) and variable names (n)

  varlist <- list(
    fe_iea, fe_weo, fe_hre, pe_iea, pe_weo, trade, pop, gdpp_James,
    gdpp_WB, gdpp_IMF, ceds, edgar6, primap, cdiac, LU_EDGAR_LU, LU_CEDS,
    LU_FAO_EmisLUC, LU_FAO_EmisAg, LU_PRIMAPhist, IRENAcap, Ember, eurostat,
    # emiMktES, emiMktETS, emiMktESOthers,
    EU_ReferenceScenario, emiEurostat, ARIADNE_ReferenceScenarioGdp,
    ARIADNE_ReferenceScenarioGdpCorona, ARIADNE_ReferenceScenarioPop,
    EEA_GHGSectoral, EEA_GHGTotal, EEA_GHGProjections, Emi_Reference,
    # EEA_GHGES,
    IEA_EVOutlook, INNOPATHS, JRC_Industry, JRC_Transport, JRC_ResCom, AGEB_Bal,
    AGEB_SE, UBA_emi, UNFCCC, BP, worldsteel, steelStock, USGS_cement
  )

  y <- Reduce(union, lapply(varlist, getYears))
  n <- Reduce(c, lapply(varlist, getNames))
  y <- sort(y)

  # create empty object with full temporal, regional and data dimensionality
  data <- new.magpie(getRegions(fe_iea), y, n, fill = NA)
  getSets(data)[3] <- "model"
  getSets(data)[4] <- "variable"

  # transfer data of existing years
  for (i in varlist) {
    data[, getYears(i), getNames(i)] <- i
  }

  # add scenario dimension ====
  data <- add_dimension(data, dim = 3.1, add = "scenario", nm = "historical")
  # rename dimension "data" into "variable"
  getSets(data)[5] <- "variable"

  # rename emission variables generated by calcValidEmissions (magpie) to the names generated by calcEmissions (remind)
  # note: spelling for the same gas might be different across historical sources
  getNames(data) <- gsub("Emissions|CO2|Land|+|Land Use Change", "Emi|CO2|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|CO2|Land|+|Land-use Change", "Emi|CO2|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|CH4|Land|Agriculture", "Emi|CH4|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|CH4|Land|+|Agriculture", "Emi|CH4|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|N2O|Land|Agriculture", "Emi|N2O|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|N2O|Land|+|Agriculture", "Emi|N2O|Land Use", getNames(data), fixed = TRUE)

  # change unit from Mt to kt for N2O from calcValidEmissions (magpie)
  vars_with_unit_Mt <- getNames(data[, , "(Mt N2O/yr)", pmatch = T])
  data[, , vars_with_unit_Mt] <- data[, , vars_with_unit_Mt] * 1000
  getNames(data) <- gsub("(Mt N2O/yr)", "(kt N2O/yr)", getNames(data), fixed = TRUE)

  return(list(x = data, weight = NULL, unit = "Various", description = "Historical Data"))
}
