#' Calculate REMIND emission variables from historical UNFCCC values
#'
#' @return A magpie object.
#'
#' @author Falk Benke, Pascal Weigmann
#' @importFrom dplyr select mutate left_join
#'
#' @param subtype either generate for countries "all" or "annex-1-only"
#'
calcUNFCCC <- function(subtype = "all") {
  stopifnot(subtype %in% c("all", "annex-1-only"))

  data <- readSource("UNFCCC", subtype = "annex-1")

  # map to REMIND variables

  mapping <- toolGetMapping("Mapping_UNFCCC.csv", type = "reportingVariables", where = "mrremind") %>%
    select("variable" = "UNFCCC", "REMIND", "conversion" = "Factor", "Unit_REMIND") %>%
    mutate("REMIND" = trimws(.data$REMIND),
           "variable" = trimws(gsub("\\.", "_", .data$variable))) %>%
    filter(!is.na(.data$REMIND), .data$REMIND != "")

  df <- data %>%
    mselect(variable = unique(mapping$variable)) %>%
    quitte::as.quitte(na.rm = TRUE) %>%
    filter(.data$period >= 1990)

  x <- left_join(df, mapping, by = "variable", relationship = "many-to-many") %>%
    mutate(
      "value" = .data$value * .data$conversion,
      "REMIND" = paste0(.data$REMIND, " (", .data$Unit_REMIND, ")")
    ) %>%
    select("variable" = "REMIND", "region", "period", "value")

  x <- stats::aggregate(value ~ variable + region + period, x, sum) %>%
    as.magpie()

  # fill missing values with 0, because there are assumed to be small
  x[is.na(x)] <- 0

  # aggregate by gas, structure:
  # Emi|{gas}|w/o Bunkers|LULUCF national accounting ({unit} {gas}/yr) =
  #    Emi|{gas}|Agriculture ({unit} {gas}/yr) +
  #    Emi|{gas}|w/o Bunkers|Energy ({unit} {gas}/yr) +
  #    Emi|{gas}|Industrial Processes ({unit} {gas}/yr) +
  #    Emi|{gas}|Land-Use Change|LULUCF national accounting ({unit} {gas}/yr) +
  #    Emi|{gas}|Waste ({unit} {gas}/yr)
  .addAggregateByGasColumn <- function(x, gas, unit) {
    newCol <- glue::glue("Emi|{gas}|w/o Bunkers|LULUCF national accounting ({unit} {gas}/yr)")
    x <- add_columns(x, newCol, dim = 3.1)

    x[, , newCol] <- dimSums(
      x[, , c(
        glue::glue("Emi|{gas}|Agriculture ({unit} {gas}/yr)"),
        glue::glue("Emi|{gas}|w/o Bunkers|Energy ({unit} {gas}/yr)"),
        glue::glue("Emi|{gas}|Industrial Processes ({unit} {gas}/yr)"),
        glue::glue("Emi|{gas}|Land-Use Change|LULUCF national accounting ({unit} {gas}/yr)"),
        glue::glue("Emi|{gas}|Waste ({unit} {gas}/yr)")
      )],
      dim = 3, na.rm = TRUE
    )
    return(x)
  }
  x <- .addAggregateByGasColumn(x, "CH4", "Mt") # Emi|CH4|w/o Bunkers|LULUCF national accounting (Mt CH4/yr)
  x <- .addAggregateByGasColumn(x, "CO2", "Mt") # Emi|CO2|w/o Bunkers|LULUCF national accounting (Mt CO2/yr)
  x <- .addAggregateByGasColumn(x, "N2O", "kt") # Emi|N2O|w/o Bunkers|LULUCF national accounting (kt N2O/yr)


  # additional CO2 variables ----

  x <- add_columns(x, "Emi|CO2|w/ Bunkers|LULUCF national accounting (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|w/ Bunkers|LULUCF national accounting (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|w/o Bunkers|LULUCF national accounting (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Energy|Demand|Transport|International Bunkers (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|CO2|w/ Bunkers|Energy (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|w/ Bunkers|Energy (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|w/o Bunkers|Energy (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Energy|Demand|Transport|International Bunkers (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|CO2|w/o Bunkers|Energy and Industrial Processes (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|w/o Bunkers|Energy and Industrial Processes (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|w/o Bunkers|Energy (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Industrial Processes (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|CO2|w/ Bunkers|Energy and Industrial Processes (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|w/ Bunkers|Energy and Industrial Processes (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|w/o Bunkers|Energy and Industrial Processes (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Energy|Demand|Transport|International Bunkers (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|CO2|w/o Bunkers|Energy|Demand (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|w/o Bunkers|Energy|Demand (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|Energy|Demand|Industry (Mt CO2/yr)"] +
    x[, , "Emi|CO2|w/o Bunkers|Energy|Demand|Transport (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Energy|Demand|Buildings (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|CO2|w/ Bunkers|Energy|Demand (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|w/ Bunkers|Energy|Demand (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|w/o Bunkers|Energy|Demand (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Energy|Demand|Transport|International Bunkers (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|CO2|w/ Bunkers|Energy|Demand|Transport (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|w/ Bunkers|Energy|Demand|Transport (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|w/o Bunkers|Energy|Demand|Transport (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Energy|Demand|Transport|International Bunkers (Mt CO2/yr)"]

  # aggregate by sector (gases weighted by GWP), structure:
  # Emi|GHG|{sector} (Mt CO2eq/yr) =
  #    Emi|CO2|{sector} (Mt CO2/yr) +
  #    Emi|CH4|{sector} (Mt CH4/yr) +
  #    Emi|N2O|{sector} (kt N2O/yr)
  .addAggregateBySectorColumn <- function(x, sector) {
    new_col <- glue::glue("Emi|GHG|{sector} (Mt CO2eq/yr)")
    x <- add_columns(x, new_col, dim = 3.1)
    gwpCH4 <- 28
    gwpN2O <- 265
    x[, , new_col] <-
      x[, , glue::glue("Emi|CO2|{sector} (Mt CO2/yr)")] +
      x[, , glue::glue("Emi|CH4|{sector} (Mt CH4/yr)")] * gwpCH4 +
      x[, , glue::glue("Emi|N2O|{sector} (kt N2O/yr)")] / 1000 * gwpN2O
    return(x)
  }

  # Emi|GHG|w/o Bunkers|Energy (Mt CO2eq/yr)
  x <- .addAggregateBySectorColumn(x, "w/o Bunkers|Energy")
  # Emi|GHG|Industrial Processes (Mt CO2eq/yr)
  x <- .addAggregateBySectorColumn(x, "Industrial Processes")
  # Emi|GHG|Agriculture (Mt CO2eq/yr)
  x <- .addAggregateBySectorColumn(x, "Agriculture")
  # Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)
  x <- .addAggregateBySectorColumn(x, "Land-Use Change|LULUCF national accounting")
  # Emi|GHG|Waste (Mt CO2eq/yr)
  x <- .addAggregateBySectorColumn(x, "Waste")
  # Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)
  x <- .addAggregateBySectorColumn(x, "w/o Bunkers|LULUCF national accounting")
  # Emi|GHG|Energy|Demand|Transport|International Bunkers (Mt CO2eq/yr)
  x <- .addAggregateBySectorColumn(x, "Energy|Demand|Transport|International Bunkers")

  # additional GHG variables ----
  x <- add_columns(x, "Emi|GHG|w/ Bunkers|LULUCF national accounting (Mt CO2eq/yr)", dim = 3)
  x[, , "Emi|GHG|w/ Bunkers|LULUCF national accounting (Mt CO2eq/yr)"] <-
    x[, , "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"] +
    x[, , "Emi|GHG|Energy|Demand|Transport|International Bunkers (Mt CO2eq/yr)"]


  x <- add_columns(x, "Emi|GHG|w/ Bunkers|Energy (Mt CO2eq/yr)", dim = 3)
  x[, , "Emi|GHG|w/ Bunkers|Energy (Mt CO2eq/yr)"] <-
    x[, , "Emi|GHG|w/o Bunkers|Energy (Mt CO2eq/yr)"] +
    x[, , "Emi|GHG|Energy|Demand|Transport|International Bunkers (Mt CO2eq/yr)"]

  x <- add_columns(x, "Emi|GHG|w/o Bunkers|Energy and Industrial Processes (Mt CO2eq/yr)", dim = 3)
  x[, , "Emi|GHG|w/o Bunkers|Energy and Industrial Processes (Mt CO2eq/yr)"] <-
    x[, , "Emi|GHG|w/o Bunkers|Energy (Mt CO2eq/yr)"] +
    x[, , "Emi|GHG|Industrial Processes (Mt CO2eq/yr)"]

  x <- add_columns(x, "Emi|GHG|w/ Bunkers|Energy and Industrial Processes (Mt CO2eq/yr)", dim = 3)
  x[, , "Emi|GHG|w/ Bunkers|Energy and Industrial Processes (Mt CO2eq/yr)"] <-
    x[, , "Emi|GHG|w/o Bunkers|Energy and Industrial Processes (Mt CO2eq/yr)"] +
    x[, , "Emi|GHG|Energy|Demand|Transport|International Bunkers (Mt CO2eq/yr)"]

  # Emi|GHG|Energy|Demand|Industry (Mt CO2eq/yr)
  x <- .addAggregateBySectorColumn(x, "Energy|Demand|Industry")
  # Emi|GHG|w/o Bunkers|Energy|Demand|Transport (Mt CO2eq/yr)
  x <- .addAggregateBySectorColumn(x, "w/o Bunkers|Energy|Demand|Transport")
  # Emi|GHG|Energy|Demand|Buildings (Mt CO2eq/yr)
  x <- .addAggregateBySectorColumn(x, "Energy|Demand|Buildings")

  x <- add_columns(x, "Emi|GHG|w/o Bunkers|Energy|Demand (Mt CO2eq/yr)", dim = 3)
  x[, , "Emi|GHG|w/o Bunkers|Energy|Demand (Mt CO2eq/yr)"] <-
    x[, , "Emi|GHG|Energy|Demand|Industry (Mt CO2eq/yr)"] +
    x[, , "Emi|GHG|w/o Bunkers|Energy|Demand|Transport (Mt CO2eq/yr)"] +
    x[, , "Emi|GHG|Energy|Demand|Buildings (Mt CO2eq/yr)"]

  x <- add_columns(x, "Emi|GHG|w/ Bunkers|Energy|Demand (Mt CO2eq/yr)", dim = 3)
  x[, , "Emi|GHG|w/ Bunkers|Energy|Demand (Mt CO2eq/yr)"] <-
    x[, , "Emi|GHG|w/o Bunkers|Energy|Demand (Mt CO2eq/yr)"] +
    x[, , "Emi|GHG|Energy|Demand|Transport|International Bunkers (Mt CO2eq/yr)"]

  x <- add_columns(x, "Emi|GHG|w/ Bunkers|Energy|Demand|Transport (Mt CO2eq/yr)", dim = 3)
  x[, , "Emi|GHG|w/ Bunkers|Energy|Demand|Transport (Mt CO2eq/yr)"] <-
    x[, , "Emi|GHG|w/o Bunkers|Energy|Demand|Transport (Mt CO2eq/yr)"] +
    x[, , "Emi|GHG|Energy|Demand|Transport|International Bunkers (Mt CO2eq/yr)"]

  x <- add_columns(x, "Emi|CO2|Industry (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|Industry (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|Industrial Processes (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Energy|Demand|Industry (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|CO2|Industry|Cement (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|Industry|Cement (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|Industrial Processes|Cement (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Energy|Demand|Industry|Cement (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|CO2|Industry|Steel (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|Industry|Steel (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|Industrial Processes|Steel (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Energy|Demand|Industry|Steel (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|CO2|Industry|Chemicals (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|Industry|Chemicals (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|Industrial Processes|Chemicals (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Energy|Demand|Industry|Chemicals (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|CO2|Industry|Other Industry (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|Industry|Other Industry (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|Industrial Processes (Mt CO2/yr)"] -
    x[, , "Emi|CO2|Industrial Processes|Chemicals (Mt CO2/yr)"] -
    x[, , "Emi|CO2|Industrial Processes|Steel (Mt CO2/yr)"] -
    x[, , "Emi|CO2|Industrial Processes|Cement (Mt CO2/yr)"] +
    x[, , "Emi|CO2|Energy|Demand|Industry|Other Industry (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|GHG|Industry (Mt CO2eq/yr)", dim = 3)
  x[, , "Emi|GHG|Industry (Mt CO2eq/yr)"] <-
    x[, , "Emi|GHG|Industrial Processes (Mt CO2eq/yr)"] +
    x[, , "Emi|GHG|Energy|Demand|Industry (Mt CO2eq/yr)"]

  # overall aggregates ----
  x <- add_columns(x, "Emi|CO2|w/o Bunkers|w/o Land-Use Change (Mt CO2/yr)", dim = 3)
  x[, , "Emi|CO2|w/o Bunkers|w/o Land-Use Change (Mt CO2/yr)"] <-
    x[, , "Emi|CO2|w/o Bunkers|LULUCF national accounting (Mt CO2/yr)"] -
    x[, , "Emi|CO2|Land-Use Change|LULUCF national accounting (Mt CO2/yr)"]

  x <- add_columns(x, "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)", dim = 3)
  x[, , "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"] <-
    x[, , "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"] -
    x[, , "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"]

  .aliasColumn <- function(x, aliasName, existingColumn) {
    stopifnot(existingColumn %in% getItems(x, dim = 3))
    x <- add_columns(x, aliasName, dim = 3)
    x[, , aliasName] <-
      x[, , existingColumn]
    return(x)
  }

  # default variables equal to "w/ bunkers" variables
  x <- .aliasColumn(x, "Emi|CO2|LULUCF national accounting (Mt CO2/yr)",
                    "Emi|CO2|w/ Bunkers|LULUCF national accounting (Mt CO2/yr)")
  x <- .aliasColumn(x, "Emi|CO2|Energy (Mt CO2/yr)",
                    "Emi|CO2|w/ Bunkers|Energy (Mt CO2/yr)")
  x <- .aliasColumn(x, "Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)",
                    "Emi|CO2|w/ Bunkers|Energy and Industrial Processes (Mt CO2/yr)")
  x <- .aliasColumn(x, "Emi|CO2|Energy|Demand (Mt CO2/yr)",
                    "Emi|CO2|w/ Bunkers|Energy|Demand (Mt CO2/yr)")
  x <- .aliasColumn(x, "Emi|CO2|Energy|Demand|Transport (Mt CO2/yr)",
                    "Emi|CO2|w/ Bunkers|Energy|Demand|Transport (Mt CO2/yr)")


  x <- .aliasColumn(x, "Emi|GHG|LULUCF national accounting (Mt CO2eq/yr)",
                    "Emi|GHG|w/ Bunkers|LULUCF national accounting (Mt CO2eq/yr)")
  x <- .aliasColumn(x, "Emi|GHG|Energy (Mt CO2eq/yr)",
                    "Emi|GHG|w/ Bunkers|Energy (Mt CO2eq/yr)")
  x <- .aliasColumn(x, "Emi|GHG|Energy and Industrial Processes (Mt CO2eq/yr)",
                    "Emi|GHG|w/ Bunkers|Energy and Industrial Processes (Mt CO2eq/yr)")
  x <- .aliasColumn(x, "Emi|GHG|Energy|Demand (Mt CO2eq/yr)",
                    "Emi|GHG|w/ Bunkers|Energy|Demand (Mt CO2eq/yr)")
  x <- .aliasColumn(x, "Emi|GHG|Energy|Demand|Transport (Mt CO2eq/yr)",
                    "Emi|GHG|w/ Bunkers|Energy|Demand|Transport (Mt CO2eq/yr)")

  # remove years before 1990 due to incomplete data
  x <- x[, seq(1986, 1989, 1), , invert = TRUE]



  # Use Non-Annex-1 Data ----
  if (subtype == "all") {
    nonAnnexData <- readSource("UNFCCC", subtype = "non-annex-1")

    mapping <- toolGetMapping("Mapping_UNFCCC_Non_Annex.csv", type = "reportingVariables", where = "mrremind") %>%
      select("variable" = "UNFCCC", "REMIND", "conversion" = "Factor", "Unit_REMIND") %>%
      mutate(
        "REMIND" = trimws(.data[["REMIND"]])
      ) %>%
      filter(!is.na(.data[["REMIND"]]), .data[["REMIND"]] != "")

    nonAnnexData <- nonAnnexData %>%
      mselect(variable = unique(mapping[["variable"]])) %>%
      as.data.frame(rev = 3) %>%
      filter(!is.na(.data[[".value"]]))

    # mapping and conversion
    nonAnnexData <- left_join(nonAnnexData, mapping, by = "variable", relationship = "many-to-many") %>%
      mutate(
        "value" = .data[[".value"]] * .data[["conversion"]],
        "REMIND" = paste0(.data[["REMIND"]], " (", .data[["Unit_REMIND"]], ")")
      ) %>%
      select("variable" = "REMIND", "region", "year", "value") %>%
      as.magpie()

    # the countries should not overlap
    stopifnot(length(intersect(getItems(x, dim = 1), getItems(nonAnnexData, dim = 1))) == 0)

    years = union(getYears(x), getYears(nonAnnexData))
    names = union(getNames(x), getNames(nonAnnexData))

    # write both annex and non-annex data into a new magclass object
    result <- new.magpie(
      cells_and_regions = getISOlist(),
      years = years,
      names = names,
      sets = c("region", "year", "variable"),
      fill = NA
    )
    result[getItems(x, dim = 1), getYears(x), getNames(x)] <- x
    result[getItems(nonAnnexData, dim = 1), getYears(nonAnnexData), getNames(nonAnnexData)] <- nonAnnexData
  } else {
    result <- x %>%
      toolCountryFill(fill = NA, verbosity = 2)
  }

  # fill countries of selected regions with 0 to allow for regional aggregation
  regions.fill <- c("EUR", "REF", "NEU", "CAZ")
  mapping <- toolGetMapping("regionmappingH12.csv",
                            type = "regional",
                            where = "mappingfolder") %>%
    filter(.data$RegionCode %in% regions.fill)

  tmp <- result[unique(mapping$CountryCode), , ]
  tmp[is.na(tmp)] <- 0
  result[unique(mapping$CountryCode), , ] <- tmp

  return(list(
    x = result, weight = NULL,
    unit = c("Mt CO2", "Mt CH4", "kt N2O", "Mt CO2eq"),
    description = "Historical UNFCCC values as REMIND variables"
  ))
}
