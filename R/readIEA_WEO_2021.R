#' IEA WEO 2021 Data
#' @description  IEA WEO 2021 Data. See README in input file for more details.
#'
#' @return A [`magpie`][magclass::magclass] object.
#' @author Falk Benke
#' @importFrom dplyr filter %>% distinct group_by ungroup rename_all
#' @importFrom rlang sym
#'

readIEA_WEO_2021 <- function() { # nolint

  data <- rbind(
    read.csv2(
      file = "WEO2021_Extended_Data_Regions.csv",
      sep = ","
    ) %>% rename_all(tolower),
    read.csv2(
      file = "WEO2021_Extended_Data_Supply_Refining_Trade_Prices.csv",
      sep = ","
    ) %>% rename_all(tolower),
    read.csv(
      file = "WEO2021_Extended_Data_World.csv",
      sep = ","
    ) %>% rename_all(tolower)
  ) %>%
    mutate(
      year = as.numeric(!!sym("year")),
      value = as.numeric(!!sym("value"))
    ) %>%
    select("region", "year", "scenario", "category", "product", "flow", "unit", "value") %>%
    group_by(
      !!sym("region"), !!sym("year"), !!sym("scenario"), !!sym("category"),
      !!sym("product"), !!sym("flow"), !!sym("unit")
    ) %>%
    distinct() %>%
    ungroup()

  as.magpie(data, temporal = 2, spatial = 1, datacol = 8) %>%
    magpiesort() %>%
    return()
}
