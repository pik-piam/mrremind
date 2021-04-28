#' Calculate REMIND final energy variables from historical AGEB values
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate left_join
#' @importFrom madrat toolGetMapping toolCountryFill
#' @importFrom magclass as.magpie mselect
#' @importFrom rlang sym
#' @importFrom stats aggregate
#' @export
calcAGEB <- function() {
  ageb <- readSource("AGEB")

  mapping <- toolGetMapping("Mapping_AGEB_REMIND.csv", type = "reportingVariables") %>%
    mutate(!!sym("conversion") := as.numeric(!!sym("Factor")) * !!sym("Weight")) %>%
    select("variable" = "AGEB_variable", "REMIND_variable", "conversion", "unit" = "Unit_AGEB", "Unit_REMIND")

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
    by = "variable"
  ) %>%
    mutate(
      !!sym("value") := ifelse(
        is.na(!!sym("value")), 0, !!sym("value") * !!sym("conversion")
      ),
      !!sym("REMIND_variable") := paste0(!!sym("REMIND_variable"), " (", !!sym("Unit_REMIND"), ")")
    ) %>%
    select("variable" = "REMIND_variable", "region", "year", "value")

  x <- aggregate(value ~ variable + region + year, x, sum) %>%
    as.magpie() %>%
    toolCountryFill(fill = 0)

  return(list(
    x = x, weight = NULL,
    unit = "EJ/yr",
    description = "Historical AGEB values as REMIND variables"
  ))
}
