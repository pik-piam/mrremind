#' #' This function reads WACC markup data across regions and periods,
#' computing the average value across technologies for each region and period
#' as a proxy for the macroeconomic cost of capital for each region
#' @author Diamantis Koutsandreas

readETH_COUN <- function() {
  years <- c(
    2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050,
    2055, 2060, 2070, 2080, 2090, 2100, 2110, 2130, 2150
  )
  data <- readxl::read_xlsx("Source_data_WACC.xlsx",
    sheet = "wacc_markups",
    range = "A1:CH13"
  ) %>%
    tidyr::pivot_longer(cols = 2:86, names_to = "tewacc", values_to = "value") %>%
    dplyr::rename(reg = 1)
  data_expanded <- data %>%
    dplyr::slice(rep(seq_len(dplyr::n()), each = length(years))) %>%
    dplyr::mutate(t = rep(years, times = nrow(data)))
  data_averaged <- data_expanded %>%
    dplyr::group_by(.data$t, .data$reg) %>%
    dplyr::summarise(value = mean(.data$value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::select("t", "reg", "value")
  return(as.magpie(data_averaged))
}
