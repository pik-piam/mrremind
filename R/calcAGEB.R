#' Calculate REMIND final energy variables from historical AGEB values
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @param subtype data subtype. Either "balances" ("Auswertungstabellen zur Energiebilanz Deutschland") or
#'        "electricity" ("Bruttostromerzeugung in Deutschland nach Energieträgern")
#' @importFrom dplyr select mutate left_join
#' @importFrom stats aggregate
#' @export
calcAGEB <- function(subtype = "balances") {
  ageb <- readSource("AGEB", subtype = subtype)

  mapping <- toolGetMapping("Mapping_AGEB_REMIND.csv", type = "reportingVariables", where = "mrremind") %>%
    mutate("conversion" = as.numeric(.data$Factor)) %>%
    select("variable" = "AGEB_variable", "REMIND_variable", "conversion", "unit" = "Unit_AGEB", "Unit_REMIND") %>%
    filter(.data$REMIND_variable != "")

  x <- left_join(
    ageb %>%
      mselect(region = "DEU", variable = unique(mapping$variable)) %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(
        "region" = "Region", "year" = "Year", "variable" = "Data1",
        "unit" = "Data2", "value" = "Value"
      ),
    mapping,
    by = "variable", relationship = "many-to-many"
  ) %>%
    mutate(
      "value" = .data$value * .data$conversion,
      "REMIND_variable" = paste0(.data$REMIND_variable, " (", .data$Unit_REMIND, ")")
    ) %>%
    select("variable" = "REMIND_variable", "region", "year", "value")

  x <- aggregate(value ~ variable + region + year, x, sum) %>%
    as.magpie() %>%
    toolCountryFill(fill = NA, verbosity = 2)

  return(list(
    x = x, weight = NULL, unit = "EJ/yr",
    description = "Historical AGEB values as REMIND variables"
  ))
}
