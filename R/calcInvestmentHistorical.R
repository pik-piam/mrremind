#' Calculate REMIND investment variables from IEA World Energy Investment Outlook (2024)
#' @author Nicolas Bauer, Falk Benke
#'
calcInvestmentHistorical <- function() {
  x <- readSource("IEA_WEIO")[c("World", "China"), , ]
  getItems(x, dim = 1) <- c("GLO", "CHN")

  mapping <- toolGetMapping("Mapping_IEA_WEIO.csv", type = "reportingVariables", where = "mrremind") %>%
    filter(.data$variable != "")

  x <- toolAggregate(x, rel = mapping, from = "IEA", to = "variable", dim = 3, partrel = TRUE, verbosity = 2)
  getNames(x) <- paste0(getNames(x), " (constant 2023 USD MER)")

  return(list(
    x = x,
    weight = NULL,
    unit = "constant 2023 USD MER",
    description = "Investment data from IEA World Energy Investment Outlook (2024)",
    isocountries = FALSE
  ))
}
