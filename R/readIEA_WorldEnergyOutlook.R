#' Read in IEA World Energy Outlook Data from 2021 or 2023
#'
#' @param subtype "2021-global", "2021-region", "2023-global", or "2023-region".
#' - For 2021 we have complete paid data. For 2023 we have only the free dataset.
#' - On global level, the source offers more variables than on regional level,
#' but the data should not be used on sub-global level due to its coarse disaggregation.
#' @author Falk Benke
#' @importFrom dplyr filter distinct group_by ungroup rename_all
#'
readIEA_WorldEnergyOutlook <- function(subtype = "2021-region") { # nolint

  if (grepl("2021-", subtype)) {

    data <- rbind(
      read.csv2(
        file = "2021/complete/WEO2021_Extended_Data_Regions.csv",
        sep = ","
      ) %>% rename_all(tolower),
      read.csv2(
        file = "2021/complete/WEO2021_Extended_Data_Supply_Refining_Trade_Prices.csv",
        sep = ","
      ) %>% rename_all(tolower),
      read.csv(
        file = "2021/complete/WEO2021_Extended_Data_World.csv",
        sep = ","
      ) %>% rename_all(tolower),
      read.csv(
        file = "2021/complete/WEO2021_Extended_Data_Investment.csv",
        sep = ","
      ) %>% rename_all(tolower)
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
    rangeData <- filter(
      data, is.na(suppressWarnings(as.numeric(.data$year))),
      grepl("Investment spending, annual average", .data$variable)
    )

    # remove non-annual data
    data <- filter(data, !is.na(suppressWarnings(as.numeric(.data$year))))

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

  } else if (grepl("2023-", subtype)) {

    data <- rbind(
      read.csv2(
        file = "2023/free/WEO2023_AnnexA_Free_Dataset_Regions.csv",
        sep = ","
      ) %>% rename_all(tolower),
      read.csv2(
        file = "2023/free/WEO2023_AnnexA_Free_Dataset_World.csv",
        sep = ","
      ) %>% rename_all(tolower)
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

    as.magpie(data, temporal = 2, spatial = 1, datacol = 5) %>%
      magpiesort() %>%
      return()

  } else {
    stop("Not a valid subtype! Must be one of: '2021-region', '2021-global', '2023-region', '2023-global'")
  }
}
