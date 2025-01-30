#' Read in IEA World Energy Outlook Data from 2023
#'
#' @author Falk Benke
#' @importFrom dplyr filter distinct group_by ungroup
#'
readIEA_WorldEnergyOutlook <- function() { # nolint

  data <- rbind(
   utils::read.csv2(
      file = "2023/complete/WEO2023_Extended_Data_Regions.csv",
      sep = ","
    ) %>% dplyr::rename_all(tolower),
   utils::read.csv2(
      file = "2023/complete/WEO2023_Extended_Data_Supply_Refining_H2_Trade_Prices.csv",
      sep = ","
    ) %>% dplyr::rename_all(tolower),
    read.csv(
      file = "2023/complete/WEO2023_Extended_Data_World.csv",
      sep = ","
    ) %>% dplyr::rename_all(tolower),
    read.csv(
      file = "2023/complete/WEO2023_Extended_Data_Investment.csv",
      sep = ","
    ) %>% dplyr::rename_all(tolower)
  ) %>%
    mutate(
      "value" = ifelse(.data$unit == "PJ", as.numeric(.data$value) / 1000, as.numeric(.data$value)),
      "unit" = ifelse(.data$unit == "PJ", "EJ", .data$unit),
      "variable" = paste0(.data$category, "-", .data$product, "-", .data$flow, " (", .data$unit, ")")
    ) %>%
    select("region", "year", "scenario", "variable", "value") %>%
    group_by(.data$region, .data$year, .data$scenario, .data$variable) %>%
    distinct() %>%
    ungroup()

  # investment data uses yearly ranges and needs special treatment
  # we currently don't read in cumulative investment spending, only annual average spending
  rangeData <- data %>% filter(
    is.na(suppressWarnings(as.numeric(.data$year))),
    grepl("Investment spending, annual average", .data$variable)
  )

  # remove non-annual data
  data <- data %>%
    filter(!is.na(suppressWarnings(as.numeric(.data$year))))

  years <- as.numeric(unique(data$year))

  splitData <- data.frame()

  for (i in seq(1, nrow(rangeData))) {
    d <- rangeData[i, ]
    minY <- as.numeric(sub("-[0-9]{4}", "", d[, "year"]))
    maxY <- as.numeric(sub("[0-9]{4}-", "", d[, "year"]))
    y <- data.frame(year = seq(minY, maxY, 1), variable = d[, "variable"]) %>%
      filter(.data$year %in% years)
    splitData <- rbind(splitData, left_join(y, select(d, -2), by = "variable"))
  }

  data <- rbind(data, splitData)

  as.magpie(data, temporal = 2, spatial = 1, datacol = 5) %>%
    magpiesort() %>%
    return()

}
