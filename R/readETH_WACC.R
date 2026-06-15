#' This function reads WACC markup data across technologies for each of
#' the REMIND regions
#' @author Diamantis Koutsandreas

readETH_WACC <- function() {
  years <- c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050,
             2055, 2060, 2070, 2080, 2090, 2100, 2110, 2130, 2150)

  data <- readxl::read_xlsx("Source_data_WACC.xlsx",
                            sheet = "wacc_markups",
                            range = "A1:CH13") %>%
    pivot_longer(cols = 2:86, names_to = "tewacc", values_to = "value") %>%
    rename(reg = 1) # Rename the region column to 'reg'

  data_expanded <- data %>%
    dplyr::slice(rep(seq_len(dplyr::n()), each = length(years))) %>%
    mutate(t = rep(years, times = nrow(data)))

  data_expanded <- data_expanded %>%
    select("t", "reg", "tewacc", "value")

  return(as.magpie(data_expanded))
}




