#' Calculate REMIND final energy variables from historical UBA values
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate left_join filter
#' @importFrom rlang sym
#' @importFrom stats aggregate
#' @export
calcIEA_InvestDatav2024 <- function() {
  investData <- readSource("IEA_InvestDatav2024")
  investData <- investData[c("World", "China"),,]
  getItems(investData, dim = 1) <- c("GLO", "CHN")

  x <- as.quitte(x)

  mapping <- toolGetMapping("Mapping_IEAinvest_REMIND.csv", type = "reportingVariables", where = "mrremind") %>%
    select("variable" = "IEA_variable", "REMIND_variable", "Unit_REMIND") %>%
    filter(!!sym("REMIND_variable") != "")

  x <- left_join(
    investData %>%
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
    mutate("REMIND_variable" = paste0(!!sym("REMIND_variable"), " (", !!sym("Unit_REMIND"), ")")) %>%
    select("variable" = "REMIND_variable", "region", "year", "value")

  x <- aggregate(value ~ variable + region + year, x, sum) %>%
    as.magpie()  %>%
    toolCountryFill(fill = NA, verbosity = 2)

  return(list(
    x = x, weight = NULL,
    unit = c("Mt CO2/yr", "Mt CO2-equiv/yr"),
    description = "IEA investment data"
  ))
}
