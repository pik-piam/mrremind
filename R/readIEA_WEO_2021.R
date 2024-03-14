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
      file = "complete/WEO2021_Extended_Data_Regions.csv",
      sep = ","
    ) %>% rename_all(tolower),
    read.csv2(
      file = "complete/WEO2021_Extended_Data_Supply_Refining_Trade_Prices.csv",
      sep = ","
    ) %>% rename_all(tolower),
    read.csv(
      file = "complete/WEO2021_Extended_Data_World.csv",
      sep = ","
    ) %>% rename_all(tolower),
    read.csv(
      file = "complete/WEO2021_Extended_Data_Investment.csv",
      sep = ","
    ) %>% rename_all(tolower)
  ) %>%
    mutate(
      !!sym("value") := ifelse(!!sym("unit") == "PJ", as.numeric(!!sym("value")) / 1000, as.numeric(!!sym("value"))),
      !!sym("unit") := ifelse(!!sym("unit") == "PJ", "EJ", !!sym("unit")),
      variable = paste0(!!sym("category"), "-", !!sym("product"), "-", !!sym("flow"), " (", !!sym("unit"), ")")
    ) %>%
    select("region", "year", "scenario", "variable", "value") %>%
    group_by(
      !!sym("region"), !!sym("year"), !!sym("scenario"), !!sym("variable")
    ) %>%
    distinct() %>%
    ungroup()

  # investment data uses yearly ranges and needs special treatment
  # we currently don't read in cumulative investment spending, only annual average spending
  rangeData <- filter(
    data, is.na(suppressWarnings(as.numeric(!!sym("year")))),
    grepl("Investment spending, annual average", !!sym("variable"))
  )

  # remove non-annual data
  data <- filter(data, !is.na(suppressWarnings(as.numeric(!!sym("year")))))

  years <- as.numeric(unique(data$year))

  splitData <- data.frame()

  for (i in seq(1, nrow(rangeData))) {
    d <- rangeData[i, ]
    minY <- as.numeric(sub("-[0-9]{4}", "", d[, "year"]))
    maxY <- as.numeric(sub("[0-9]{4}-", "", d[, "year"]))
    y <- data.frame(year = seq(minY, maxY, 1), variable = d[, "variable"]) %>%
      filter(!!sym("year") %in% years)
    splitData <- rbind(splitData, left_join(y, select(d, -2), by = "variable"))
  }

  data <- rbind(data, splitData)

  as.magpie(data, temporal = 2, spatial = 1, datacol = 5) %>%
    magpiesort() %>%
    return()
}
