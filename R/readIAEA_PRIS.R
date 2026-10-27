#' read IAEA Power Reactor Information System
#'
#' read Nuclear capacities and near-term outlook from data scraped from
#' https://pris.iaea.org/PRIS/CountryStatistics/CountryStatisticsLandingPage.aspx
#'
#' @author Pascal Weigmann
#'
readIAEA_PRIS <- function() {
  # only information given is what is currently operating and what is under
  # construction (without start date). all calculations are about 2030 estimates
  x <- readxl::read_xlsx("nuclear_capacities_20260608.xlsx") %>%
    mutate("operational"  = .data$operational,
           "construction" = .data$construction,
           "inactive"     = .data$inactive,  # only relevant for Japan
           variable = "Cap|Electricity|Nuclear") %>%
    # beginning 2026 value is close enough for 2022-2027 average
    mutate(year = "y2025") %>%
    tidyr::pivot_longer(cols = c("operational", "construction", "inactive"),
                        names_to = "status") %>%
    select("country", "year", "variable", "status", "unit", "value") %>%
    as.magpie(spatial = "country", temporal = "year") %>%
    add_dimension(dim = 3.1, add = "model", nm = "IAEA_PRIS")

  # move "construction" and "inactive" values to year 2030
  x <- add_columns(x, addnm = c("y2030"), dim = 2, fill = NA)
  x[, 2030, "inactive"]     <- x[, 2025, "inactive"]
  x[, 2030, "construction"] <- x[, 2025, "construction"]
  x[, 2025, c("construction", "inactive")] <- 0

  # assume no retirement (for now)
  x[, 2030, "operational"] <- x[, 2025, "operational"]

  return(x)
}
