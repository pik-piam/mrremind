#' Calculate REMIND final energy variables from historical UBA values
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate left_join filter
#' @importFrom madrat toolGetMapping toolCountryFill
#' @importFrom magclass as.magpie mselect
#' @importFrom rlang sym
#' @importFrom stats aggregate
#' @export
calcUBA <- function() {
  uba <- readSource("UBA")
  
  mapping <- toolGetMapping("Mapping_UBA_REMIND.csv", type = "reportingVariables", where = "mappingfolder") %>%
    select("variable" = "UBA_variable", "REMIND_variable", "Unit_REMIND") %>% 
    filter(!!sym("REMIND_variable") != "")
  
  x <- left_join(
    uba %>%
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
      !!sym("REMIND_variable") := paste0(!!sym("REMIND_variable"), " (", !!sym("Unit_REMIND"), ")")
    ) %>%
    select("variable" = "REMIND_variable", "region", "year", "value")
  
  x <- aggregate(value ~ variable + region + year, x, sum) %>%
    as.magpie()  %>%
    toolCountryFill(fill = NA)
  
  return(list(
    x = x, weight = NULL,
    unit = c("Mt CO2/yr", "Mt CO2-equiv/yr"),
    description = "Historical UBA values as REMIND variables"
  ))
}
