#' Generate Validation Data for REMIND
#'
#' Function that generates the historical regional dataset against which the
#' REMIND model results can be compared.
#'
#' @md
#' @param rev Unused parameter here for the pleasure of [`madrat`].
#' @author David Klein, Falk Benke
#' @seealso [`fullREMIND()`], [`readSource()`], [`getCalculations()`],
#'     [`calcOutput()`]
#' @examples
#' \dontrun{
#' fullVALIDATIONREMIND()
#' }

fullVALIDATIONREMIND <- function(rev = 0) {

  # get region mappings for aggregation ----
  # Determines all regions data should be aggregated to by examining the columns
  # of the `regionmapping` and `extramappings` currently configured.

  rel <- "global" # always compute global aggregate
  for (mapping in c(getConfig("regionmapping"), getConfig("extramappings"))) {
    columns <- setdiff(
      colnames(toolGetMapping(mapping, "regional")),
      c("X", "CountryCode")
    )

    if (any(columns %in% rel)) {
      warning(
        "The following column(s) from ", mapping,
        " exist in another mapping an will be ignored: ",
        paste(columns[columns %in% rel], collapse = ", ")
      )
    }

    rel <- unique(c(rel, columns))
  }

  columnsForAggregation <- gsub(
    "RegionCode", "region",
    paste(rel, collapse = "+")
  )

  # historical data ----
  valfile <- "historical.mif"

  # Landuse Emissions ----

  calcOutput(
    type = "HistoricalLUEmissions", file = valfile,
    aggregate = columnsForAggregation, append = FALSE,
    warnNA = FALSE, round = 5, try = FALSE,
    writeArgs = list(scenario = "historical")
  )

  # Population data from WDI ----

  pop <- calcOutput("PopulationPast", aggregate = columnsForAggregation, try = FALSE)
  getNames(pop) <- paste0("Population (million)")
  write.report(pop, file = valfile, append = TRUE, scenario = "historical", model = "WDI")

  # GDP in ppp from WDI ----

  gdp <- calcOutput("GDPPast", pastData = "WDI", aggregate = columnsForAggregation, try = FALSE) / 1000
  getNames(gdp) <- paste0("GDP|PPP (billion US$2017/yr)")
  write.report(gdp, file = valfile, append = TRUE, scenario = "historical", model = "WDI")

  # IEA Primary Energy ----

  calcOutput(
    type = "PE", subtype = "IEA", ieaVersion = "latest", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "IEA")
  )

  # World Energy Outlook 2019 Primary Energy ----

  pe <- calcOutput(type = "PE", subtype = "IEA_WEO", aggregate = columnsForAggregation, warnNA = FALSE, try = FALSE)
  pe <- collapseNames(pe[, , "Current Policies Scenario", pmatch = TRUE])
  write.report(pe, file = valfile, append = TRUE, scenario = "historical", model = "IEA WEO 2019")


  # IEA Final Energy ----

  calcOutput(
    type = "FE", source = "IEA", ieaVersion = "latest", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "IEA")
  )

  # World Energy Outlook 2019 Final Energy ----

  fe <- calcOutput(type = "FE", source = "IEA_WEO", aggregate = columnsForAggregation, warnNA = FALSE, try = FALSE)
  fe <- collapseNames(fe[, , "Current Policies Scenario", pmatch = TRUE])
  write.report(fe, file = valfile, append = TRUE, scenario = "historical", model = "IEA WEO 2019")

  # IEA Fossil Trade ----

  calcOutput(
    type = "Trade", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "IEA")
  )

  # IEA EB direct sums ----

  calcOutput(
    type = "IEA_EB_directSum", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "IEA-EB-directSum")
  )

  # AGEB ----

  calcOutput(
    type = "AGEB", subtype = "balances", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "AGEB")
  )

  calcOutput(
    type = "AGEB", subtype = "electricity", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "AGEB")
  )

  # BP ----

  calcOutput(
    type = "BP", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "BP")
  )

  # CEDS Emissions ----

  # Historical emissions from CEDS data base
  calcOutput(
    "Emissions", datasource = "CEDS2024", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "CEDS")
  )

  # Historical emissions from CEDS data base, aggregated to IAMC sectors
  calcOutput(
    "Emissions", datasource = "CEDS2024_IAMC", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "CEDS IAMC sectors")
  )

  # EDGAR6 Emissions----

  # Historical emissions from EDGAR v5.0 and v6.0
  calcOutput(
    type = "Emissions", datasource = "EDGAR6", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "EDGAR6")
  )

  # EDGAR GHG Emissions----
  # does not contain as many gases as EDGAR6
  edgar <- calcOutput(
    type = "Emissions", datasource = "EDGARghg",
    aggregate = columnsForAggregation, warnNA = FALSE,
    try = FALSE
  )

  # write all regions of non-bunker variables to report
  non_bunk <- edgar[, , "International", pmatch = TRUE, invert = TRUE]
  write.report(non_bunk, file = valfile, append = TRUE,
               scenario = "historical", model = "EDGARghg")

  # write only global values of bunker variables
  bunkers <- edgar["GLO", , "International", pmatch = TRUE]
  write.report(bunkers, file = valfile, append = TRUE,
               scenario = "historical", model = "EDGARghg")

  # Ember electricity data ----

  calcOutput(
    type = "Ember", subtype = "all", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "Ember")
  )

  # Eurostat Emission Data (env_air_gge)

  calcOutput(
    type = "EurostatEmissions", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "Eurostat env_air_gge")
  )

  # European Eurostat data ----

  calcOutput(
    type = "EuropeanEnergyDatasheets",  subtype = "EU27", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "Eurostat energy_sheets")
  )

  # EU Reference Scenario ----

  calcOutput(
    type = "EU_ReferenceScenario", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # European Environment Agency Historical GHG Emissions ----

  calcOutput(
    type = "EEAGHGEmissions", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "EEA_historical")
  )

  # European Environment Agency GHG Projections ----

  calcOutput(
    type = "EEAGHGProjections", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # European Environment Agency Emission Reference Values ----

  calcOutput(
    type = "EmiReference", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "EEA")
  )


  # Global Energy Monitor ----

  calcOutput(
    type = "GlobalEnergyMonitor", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # HRE Heat Roadmap Europe (Final Energy) ----

  calcOutput(
    type = "HRE", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # IEA ETP ----

  calcOutput(
    type = "IEA_ETP", aggregate = columnsForAggregation, file = valfile,
    append = TRUE, warnNA = FALSE, try = FALSE,
    writeArgs = list(scenario = "historical")
  )

  # IEA Global EV Outlook ----

  calcOutput(
    type = "IEA_EVOutlook", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # IEA World Energy Investment Outlook 2024
  calcOutput(
    type = "InvestmentHistorical", file = valfile,
    aggregate = FALSE, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "IEA WEIO 2024")
  )


  # IEA World Energy Outlook 2023 ----
  calcOutput(
    type = "IEA_WorldEnergyOutlook", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # IEA CCUS  ----

  calcOutput(
    type = "CCScapacity", subtype = "historical", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # IRENA Capacities  ----

  calcOutput(
    type = "IRENA", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "IRENA")
  )

  # JRC IDEES ----

  calcOutput(
    type = "JRC_IDEES", subtype = "Industry", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "JRC")
  )

  calcOutput(
    type = "JRC_IDEES", subtype = "Transport", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "JRC")
  )

  calcOutput(
    type = "JRC_IDEES", subtype = "ResCom", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "JRC")
  )

  # Mueller Steel Stock ----

  calcOutput(
    type = "HistoricalSteelStock", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "Mueller")
  )

  # Steel Production ----

  calcOutput(
    type = "HistoricalBasicMaterialProduction", subtype = "steel", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )


  # USGS Cement Production ----

  calcOutput(
    type = "HistoricalBasicMaterialProduction", subtype = "cement", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )


  # UBA Emission data ----

  calcOutput(
    type = "UBA", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "UBA")
  )

  # UNFCCC ----

  calcOutput(
    type = "UNFCCC", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # UNIDO ----

  calcOutput(
    type = "UNIDO", subtype = "INDSTAT3", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "INDSTAT3")
  )

}
