#' Read IEA CCUS data
#'
#' Reads in capacities from projects in IEA CCUS database
#'
#' @author Anne Merfort, Falk Benke
#'
#' @param subtype either `historical` for data from until 2022 or `projections`
#' for projections in 2020, 2025 and 2030
#' @importFrom dplyr %>% filter mutate select
#' @importFrom readxl read_xlsx
#'
#' @export
readIEA_CCUS <- function(subtype) {
  # project types filter applied to source
  projectTypes <- c("Full chain", "Storage", "T&S")

  data <- read_excel("IEA CCUS Projects Database 2023.xlsx",
    sheet = "CCUS Projects Database"
  ) %>%
    select(
      "project" = "Project name",
      "country" = "Country",
      "type" = "Project type",
      "start" = "Operation",
      "end" = "Suspension/decommissioning",
      "status" = "Project Status",
      "value" = "Announced capacity (high) (Mt CO2/yr)"
    ) %>%
    # remove entries without announced capacities and not matching project type
    filter(!is.na(.data$value), .data$type %in% projectTypes) %>%
    # if empty, assume project start in 2030
    # end in 2030, unless decommissioned before
    mutate(
      "start" = ifelse(is.na(.data$start), 2030, .data$start),
      "end" = ifelse(is.na(.data$end), 2030, .data$end)
    ) %>%
    # remove entries where suspension is before operation or in same year
    filter(.data$start <= .data$end)

  # manually assign shared project(s) to one country
  data[data$project == "EU2NSEA", "country"] <- "Norway"

  if (subtype == "historical") {
    hist <- data %>%
      filter(.data$start <= 2022)

    tmp <- NULL
    for (i in seq_len(nrow(hist))) {
      tmp <- rbind(
        tmp,
        data.frame(
          hist[i, c("country", "value")],
          period = seq(as.numeric(hist[i, "start"]), as.numeric(hist[i, "end"]), 1)
        )
      )
    }

    cap <- aggregate(value ~ country + period, data = tmp, FUN = sum) %>%
      filter(.data$period <= 2022) %>%
      mutate(variable = "Carbon Management|Storage")

    cap <- cap[, c("country", "period", "variable", "value")]
    x <- as.magpie(cap, spatial = 1)

  } else if (subtype == "projections") {

    statusLow <- c("Operational", "Under construction")
    statusHigh <- c(statusLow, "Planned")

    proj <- data %>%
      filter(.data$status %in% statusHigh)

    tmp <- NULL
    for (i in seq_len(nrow(proj))) {
      tmp <- rbind(
        tmp,
        data.frame(
          proj[i, c("country", "status", "value")],
          period = seq(as.numeric(proj[i, "start"]), as.numeric(proj[i, "end"]), 1)
        )
      )
    }

    capLow <- aggregate(value ~ country + period,
      data = filter(tmp, .data$status %in% statusLow),
      FUN = sum
    ) %>%
      mutate(variable = "low")

    capHigh <- aggregate(value ~ country + period,
      data = filter(tmp, .data$status %in% statusHigh),
      FUN = sum
    ) %>%
      mutate(variable = "up")

    cap <- rbind(capLow, capHigh)
    cap <- cap[, c("country", "period", "variable", "value")]
    x <- as.magpie(cap, spatial = 1)
    x <- x[, c(2020, 2025, 2030), ]
  } else {
    stop("Invalid subtype. Must be eiter `historical` or `projections`")
  }

  x[is.na(x)] <- 0

  return(x)
}
