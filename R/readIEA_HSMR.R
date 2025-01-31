#' read IEA Hydro Special Market Report
#'
#' read Hydro capacities and near-term outlook from data scraped from
#' https://www.iea.org/data-and-statistics/data-tools/hydropower-data-explorer
#'
#' @author Pascal Weigmann
#' @importFrom readxl read_xlsx
#'
#' @export
readIEA_HSMR <- function() {

  x <- readxl::read_xlsx("IEA_Hydropower_Special_Market_Report.xlsx") %>%
    # get capacities in 2030 by adding 2020 values to additions
    # subtract pumped storage as it is not part of Cap Hydro in REMIND
    mutate("2020_operational" = .data$capacity_2020,
           "2030_expected"    = .data$capacity_2020 + .data$add_expected_2030 - .data$of_that_pumped,
           "2030_accelerated" = .data$capacity_2020 + .data$add_accelerated_2030 - .data$of_that_pumped,
           "2020_pumped" = .data$cap_2020_pumped,
           "2030_pumped" = .data$cap_2020_pumped + .data$of_that_pumped,
           variable = "Cap|Electricity|Hydro") %>%
    tidyr::pivot_longer(cols = c("2020_operational", "2030_expected",
                                 "2030_accelerated", "2020_pumped", "2030_pumped"),
                        names_to = "year_scen") %>%
    select("country", "variable", "year_scen", "unit", "value") %>%
    tidyr::separate(.data$year_scen, c("year", "status")) %>%
    as.magpie(spatial = "country")

  x[, 2020, "accelerated"] <- x[, 2020, "operational"]
  x[, 2020, "expected"]    <- x[, 2020, "operational"]
  x[, 2030, "operational"] <- x[, 2020, "operational"]

  x <- add_dimension(x, dim = 3.1, add = "model", nm = "IEA_HSMR")

  return(x)
}
