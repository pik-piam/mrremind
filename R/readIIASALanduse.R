#' read IIASA Land Use Emissions
#'
#' @author Falk Benke
#' @param subtype one of "historical", "forecast2030", "forecast2035"
#'
readIIASALanduse <- function(subtype) {
  if (subtype == "historical") {
    df <- readxl::read_excel("SP2-HistoricalData.xlsx", range = "B6:AI600") %>%
      tidyr::pivot_longer(cols = seq(3, 34, 1), names_to = "period") %>%
      filter(.data$Sector == "LULUCF") %>%
      mutate("period" = as.numeric(.data$period)) %>%
      select("Country", "period", "value")
  } else if (subtype == "forecast2030") {
    df <- readxl::read_excel(
      "SP3-NDC.xlsx",
      range = "B7:F600",
      col_names = c("Country", "Sector", "BAU", "Unconditional", "Conditional")
    ) %>%
      filter(.data$Sector == "LULUCF") %>%
      mutate("period" = 2030) %>%
      select("Country", "period", "value" = "Conditional")
  } else if (subtype == "forecast2035") {
    df <- readxl::read_excel("ELEVATE T6.3 Scenario Protocol NDC and LTS information v2.xlsx",
      sheet = "NDC emission levels", range = "A4:G228", col_names = c(
        "Country", "ISO", "NDC incl/excl LULUCF",
        "2030 incl LULUCF", "2030 excl LULUCF", "2035 incl LULUCF", "2035	excl LULUCF"
      )
    ) %>%
      mutate(
        "value" = .data$`2035 incl LULUCF` - .data$`2035	excl LULUCF`,
        "period" = 2035
      ) %>%
      select("Country" = "ISO", "period", "value") %>%
      filter(!is.na(.data$Country))
  } else {
    stop("Invalid subtype")
  }


  return(as.magpie(df, spatial = 1, temporal = 2))
}
