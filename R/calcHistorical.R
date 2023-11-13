#' Gather reference data from various sources.
#' @importFrom magclass setNames getNames getSets add_columns
#' @importFrom dplyr filter group_by mutate select ungroup
#' @importFrom rlang syms
#' @importFrom tidyr complete nesting
calcHistorical <- function() {

  # Final Energy
  fe_iea <- calcOutput("FE", source = "IEA", aggregate = FALSE)
  fe_iea <- add_dimension(fe_iea, dim = 3.1, add = "model", nm = "IEA")

  fe_weo <- calcOutput("FE", source = "IEA_WEO", aggregate = FALSE)
  fe_weo <- fe_weo[, , "Current Policies Scenario", pmatch = TRUE]
  fe_weo <- collapseNames(fe_weo)
  fe_weo <- add_dimension(fe_weo, dim = 3.1, add = "model", nm = "IEA_WEO")

  # Primary Energy
  pe_iea <- calcOutput("PE", subtype = "IEA", aggregate = FALSE)
  pe_iea <- add_dimension(pe_iea, dim = 3.1, add = "model", nm = "IEA")

  pe_weo <- calcOutput("PE", subtype = "IEA_WEO", aggregate = FALSE)
  pe_weo <- pe_weo[, , "Current Policies Scenario", pmatch = TRUE]
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
  ceds <- add_dimension(ceds, dim = 3.1, add = "model", nm = "CEDS")

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

  # give ceds emissions from calcValidEmissions (magpie) a name that
  # is different from ceds emissions from calcEmissions (remind)
  getNames(LU_CEDS, dim = 1) <- "ceds_lu"

  # remove duplicates from LU_FAO_EmisAg
  LU_FAO_EmisAg <- LU_FAO_EmisAg[, , which(!duplicated(getNames(LU_FAO_EmisAg)))]

  # Capacities historical data ====

  # IRENA capacities - technologies: "csp", "geohdr", "hydro", "spv", "wind"

  # Read IRENA renewables capacity data
  IRENAcap <- readSource(type = "IRENA", subtype = "Capacity")[, , c("Concentrated solar power",
                                                                     "Geothermal", "Hydropower",
                                                                     "Solar photovoltaic", "Wind")]
  IRENAcap <- IRENAcap * 1E-03 # converting MW to GW
  mapping <- data.frame(
    IRENA_techs = c("Concentrated solar power", "Geothermal", "Hydropower", "Solar photovoltaic", "Wind"),
    REMIND_var = c("Cap|Electricity|Solar|CSP (GW)", "Cap|Electricity|Geothermal (GW)",
                   "Cap|Electricity|Hydro (GW)", "Cap|Electricity|Solar|PV (GW)",
                   "Cap|Electricity|Wind (GW)"), stringsAsFactors = FALSE
  )
  # renaming technologies to REMIND naming convention
  IRENAcap <- luscale::rename_dimnames(IRENAcap, dim = 3, query = mapping, from = "IRENA_techs", to = "REMIND_var")
  IRENAcap <- mbind(IRENAcap, setNames(IRENAcap[, , "Cap|Electricity|Solar|CSP (GW)"] +
                                         IRENAcap[, , "Cap|Electricity|Solar|PV (GW)"], "Cap|Electricity|Solar (GW)"))
  IRENAcap <- add_dimension(IRENAcap, dim = 3.1, add = "model", nm = "IRENA")

  # Region specific historical data ====

  # EEA GHG Projections
  EEA_GHGProjections <- toolFillEU34Countries(calcOutput("EEAGHGProjections", aggregate = FALSE))

  # EEA GHG Sectoral Historical Data
  EEA_GHGSectoral <- toolFillEU34Countries(readSource("EEA_EuropeanEnvironmentAgency", subtype = "sectoral"))
  EEA_GHGSectoral <- add_dimension(EEA_GHGSectoral, dim = 3.1, add = "model", nm = "EEA_historical")

  EEA_GHGTotal <- toolFillEU34Countries(readSource("EEA_EuropeanEnvironmentAgency", subtype = "total"))
  EEA_GHGTotal <- add_dimension(EEA_GHGTotal, dim = 3.1, add = "model", nm = "EEA_historical")

  # Calculate Emission Reference Values
  Emi_Reference <- toolFillEU34Countries(calcOutput("EmiReference", aggregate = FALSE))
  Emi_Reference <- add_dimension(Emi_Reference, dim = 3.1, add = "model", nm = "EEA")

  # Eurostat emissions
  EUcountries <- c("ALA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
                   "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "GGY", "HUN", "IRL",
                   "IMN", "ITA", "JEY", "LVA", "LTU", "LUX", "MLT", "NLD", "POL",
                   "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
  eurostatEmi <- readSource(type = "Eurostat", subtype = "emissions")
  eurostatEmi[getItems(eurostatEmi, dim = 1)[-which(getItems(eurostatEmi, dim = 1) %in% EUcountries)], , ] <- NA
  # set values for EU countries with no values to 0
  noData <- getItems(eurostatEmi[EUcountries, , ], dim = 1)[dimSums(abs(eurostatEmi[EUcountries, , ]),
                                                                    dim = c(2, 3), na.rm = TRUE) == 0]
  eurostatEmi[noData, , ] <- 0

  # conversion factors between CO2eq and N2O / CH4 are derived by Eurostat webtool comparison
  emiEurostat <- mbind(
    setNames(eurostatEmi[, , "CH4_native.Total (excluding memo items)"], "Emi|CH4 (Mt CH4/yr)"),
    setNames(eurostatEmi[, , "N2O_native.Total (excluding memo items)"], "Emi|N2O (kt N2O/yr)") * 1000,
    setNames(eurostatEmi[, , "GHG.Land use, land use change, and forestry (LULUCF)"],
             "Emi|GHG|Land-Use Change (Mt CO2eq/yr)"),
    setNames(eurostatEmi[, , "CO2.Land use, land use change, and forestry (LULUCF)"],
             "Emi|CO2|Land-Use Change (Mt CO2/yr)"),
    setNames(eurostatEmi[, , "CH4_native.Land use, land use change, and forestry (LULUCF)"],
             "Emi|CH4|Land-Use Change (Mt CH4/yr)"),
    setNames(eurostatEmi[, , "N2O_native.Land use, land use change, and forestry (LULUCF)"],
             "Emi|N2O|Land-Use Change (kt N2O/yr)") * 1000
  )
  emiEurostat <- add_dimension(emiEurostat, dim = 3.1, add = "model", nm = "Eurostat")

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

  # blow up to union of years ====
  # find all existing years (y) and variable names (n)

  varlist <- list(
    fe_iea, fe_weo, pe_iea, pe_weo, trade, pop, gdpp_James,
    gdpp_WB, gdpp_IMF, ceds, primap, cdiac, LU_EDGAR_LU, LU_CEDS,
    LU_FAO_EmisLUC, LU_FAO_EmisAg, LU_PRIMAPhist, IRENAcap, emiEurostat,
    EEA_GHGSectoral, EEA_GHGTotal, EEA_GHGProjections, Emi_Reference,
    worldsteel, USGS_cement
  )

  y <- Reduce(union, lapply(varlist, getYears))
  n <- Reduce(c, lapply(varlist, getNames))
  y <- sort(y)

  # create empty object with full temporal, regional and data dimensionality
  data <- new.magpie(getISOlist(), y, n, fill = NA)
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
  vars_with_unit_Mt <- getNames(data[, , "(Mt N2O/yr)", pmatch = TRUE])
  data[, , vars_with_unit_Mt] <- data[, , vars_with_unit_Mt] * 1000
  getNames(data) <- gsub("(Mt N2O/yr)", "(kt N2O/yr)", getNames(data), fixed = TRUE)

  return(list(x = data, weight = NULL, unit = "Various", description = "Historical Data"))
}
