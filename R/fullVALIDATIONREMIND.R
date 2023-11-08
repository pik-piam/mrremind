#' fullVALIDATIONREMIND
#'
#' Function that generates the historical regional dataset against which the
#' REMIND model results can be compared.
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @author David Klein
#' @seealso
#' \code{\link{fullREMIND}},\code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' fullVALIDATIONREMIND()
#' }
#'
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

  calcOutput("Historical",
    round = 5, file = valfile, aggregate = columnsForAggregation,
    append = FALSE, warnNA = FALSE, try = FALSE
  )

  # AGEB ----

  # AGEB only has DEU values and crashes when not present in regions
  if ("DEU" %in% toolGetMapping(getConfig("regionmapping"), "regional", where = "mappingfolder")[, "RegionCode"]) {

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

  }

  # BP ----

  calcOutput(
    type = "BP", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "BP")
  )

  # EDGAR Emissions----

  # Historical emissions from EDGAR v5.0 and v6.0
  calcOutput(
    type = "Emissions", datasource = "EDGAR6", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "EDGAR6")
  )

  # Ember electricity data ----

  calcOutput(
    type = "Ember", subtype = "all", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "Ember")
  )

  # European Eurostat data ----

  calcOutput(
    type = "EuropeanEnergyDatasheets",  subtype = "EU27", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "Eurostat")
  )

  # EU Reference Scenario ----

  calcOutput(
    type = "EU_ReferenceScenario", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # Global Energy Monitor ----

  calcOutput(
    type = "GlobalEnergyMonitor", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # Heat Roadmap Europe (Final Energy) ----

  calcOutput(
    type = "HRE", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # IEA ETP ----

  calcOutput(
    type = "IEA_ETP", aggregate = columnsForAggregation, file = valfile,
    append = TRUE, warnNA = FALSE, try = FALSE, isValidation = TRUE,
    writeArgs = list(scenario = "historical")
  )

  # IEA EV Outlook ----

  calcOutput(
    type = "IEA_EVOutlook", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # INNOPATHS ----

  calcOutput(
    type = "INNOPATHS", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
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

  # Steel Stock ----

  calcOutput(
    type = "SteelStock", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "Mueller")
  )

  # UBA Emission data ----

  # UBA only has DEU values and crashes when not present in regions
  if ("DEU" %in% toolGetMapping(getConfig("regionmapping"), "regional", where = "mappingfolder")[, "RegionCode"]) {
    calcOutput(
      type = "UBA", file = valfile,
      aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
      try = FALSE, writeArgs = list(scenario = "historical", model = "UBA")
    )
  }

  # UNFCCC ----

  calcOutput(
    type = "UNFCCC", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical")
  )

  # UNIDO ----

  calcOutput(
    type = "UNIDO", subtype = "INDSTAT2", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = FALSE, writeArgs = list(scenario = "historical", model = "INDSTAT2")
  )

  # WEO 2021  ----
  weo <- calcOutput(
    type = "IEA_WEO_2021", subtype = "global", aggregate = columnsForAggregation,
    warnNA = FALSE, try = FALSE
  )

  weo <- weo["GLO", , ]
  write.report(weo, file = valfile, append = TRUE, scenario = "historical")

  weo <- calcOutput(
    type = "IEA_WEO_2021", subtype = "region", aggregate = columnsForAggregation,
    warnNA = FALSE, try = FALSE
  )

  weo <- weo["GLO", , invert = TRUE]
  write.report(weo, file = valfile, append = TRUE, scenario = "historical")

  # filter variables that are too imprecise on regional level ----
  filter_historical_mif()
}
