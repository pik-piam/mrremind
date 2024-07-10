#' Calculate EEA emission projections from the two projections sources provided by EEA
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate left_join filter
#' @importFrom rlang sym
#' @export

calcEEAGHGProjections <- function() {

  mapping <- toolGetMapping(type = "sectoral",
                            name = "mappingEEAGHGProjections.csv", where = "mrremind")
  eea <- readSource("EEA_EuropeanEnvironmentAgency", subtype = "projections")

  projectionsOverview <- as.data.frame(eea) %>%
    filter(!is.na(!!sym("Value"))) %>%
    select(
      "region" = "Region", "year" = "Year", "model" = "Data1",
      "Category" = "Data2", "Gas" = "Data3", "value" = "Value"
    ) %>%
    mutate("Category" = gsub("_", ".", !!sym("Category"))) %>%
    left_join(mapping, by = c("Category", "Gas")) %>%
    filter(!is.na(!!sym("Variable"))) %>%
    select("region", "year", "model", "variable" = "Variable", "value") %>%
    quitte::calc_addVariable(
      "`Emi|GHG|Industry|ETS`" = "`Emi|GHG|Industrial Processes|ETS` + `Emi|GHG|Energy|Demand|Industry|ETS`",
      "`Emi|GHG|Industry|ESR`" = "`Emi|GHG|Industrial Processes|ESR` + `Emi|GHG|Energy|Demand|Industry|ESR`",
      "`Emi|GHG|Intl aviation in ETS|ETS`" = "`Emi|GHG|w/ Intl aviation` - `Emi|GHG`",
      completeMissing = F
    ) %>%
    mutate(
      "value" = !!sym("value") / 1000,
      "model" = paste0("EEA_", !!sym("model")),
      "variable" = paste0(!!sym("variable"), " (Mt CO2eq/yr)")
    )

  eeaDetail <- readSource("EEA_EuropeanEnvironmentAgency", subtype = "projections-detailed")
  mappingDetailed <- toolGetMapping(type = "sectoral", name = "mappingEEAGHGProjectionsDetailed.csv",
                    where = "mrremind")

  projectionsDetail <- as.data.frame(eeaDetail) %>%
    filter(!is.na(!!sym("Value"))) %>%
    select(
      "region" = "Region", "year" = "Year", "model" = "Data1",
      "Category" = "Data2", "Gas" = "Data3", "value" = "Value"
    ) %>%
    mutate("Category" = gsub("_", ".", !!sym("Category"))) %>%
    left_join(mappingDetailed, by = c("Category", "Gas")) %>%
    filter(!is.na(!!sym("Variable"))) %>%
    mutate("value" = as.numeric(!!sym("value")) * !!sym("factor")) %>%
    select("region", "year", "model", "variable" = "Variable", "value", "unit") %>%
    quitte::calc_addVariable(
      "`Emi|CO2|Industry`" = "`Emi|CO2|Energy|Demand|Industry` + `Emi|CO2|Industrial Processes`",
      "`Emi|GHG|Industry`" = "`Emi|GHG|Energy|Demand|Industry` + `Emi|GHG|Industrial Processes`",
      "`Emi|CO2|Energy|Demand|Buildings`" = "`Emi|CO2|Energy|Demand|Buildings|Residential` + `Emi|CO2|Energy|Demand|Buildings|Commercial`",
      completeMissing = F, units = c("Mt CO2/yr", "Mt CO2eq/yr", "Mt CO2/yr")
    ) %>%
    mutate(
      "variable" = paste0(!!sym("variable"), " (", !!sym("unit"), ")"),
      "model" = paste0("EEA_", !!sym("model"))
    ) %>%
    select(-"unit")

  projections <- rbind(projectionsOverview, projectionsDetail) %>%
    # filter duplicates in both sources
    distinct(
      !!sym("region"), !!sym("year"), !!sym("model"), !!sym("variable"),
      .keep_all = TRUE
    )

  x <- as.magpie(projections, spatial = 1, temporal = 2, datacol = 5) %>%
    toolCountryFill(fill = NA, verbosity = 2) %>%
    toolFillEU34Countries()

  return(list(
    x = x,
    weight = NULL,
    unit = c("Mt CO2/yr", "Mt CO2eq/yr"),
    description = "EEA emission projections"
  ))
}
