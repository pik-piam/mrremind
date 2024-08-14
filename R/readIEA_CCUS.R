#' Read IEA CCUS data
#'
#' Reads in capacities from projects in IEA CCUS database
#'
#' @author Anne Merfort, Falk Benke
#'
#' @param subtype either `historical` for data until 2023,
#' `projections` for "high" and "low" projections up to 2030 used as input-data
#'  or `pipeline` separated by status for use in formulating near-term bounds
#' @importFrom dplyr filter mutate select
#' @importFrom readxl read_xlsx
#'
#' @export
readIEA_CCUS <- function(subtype) {
  # ASSUMPTION: transport and storage are limiting factors for CCS
  # project types filter applied to source
  projectTypes <- c("Full chain", "Storage", "T&S")

  data <- read_excel("IEA CCUS Projects Database 2024.xlsx",
    sheet = "CCUS Projects Database"
  ) %>%
    select(
      "project" = "Project name",
      "country" = "Country",
      "type" = "Project type",
      "start" = "Operation",
      "end" = "Suspension/decommissioning",
      "status" = "Project Status",
      "value" = "Estimated capacity by IEA (Mt CO2/yr)"
    ) %>%
    # remove entries without announced capacities and not matching project type
    filter(!is.na(.data$value), .data$type %in% projectTypes)

  # manually assign shared project(s) to one (currently: first) country
  data[data$project == "EU2NSEA", "country"] <- "Norway"
  data[data$country == "Japan-unknown", "country"] <- "Japan"

  # correct typo in IEA Data
  data[data$country == "Lybia", "country"] <- "Libya"

  # share capacities of projects by multiple countries according to their GDP
  gdp <- calcOutput("GDP", aggregate = FALSE)
  gdp_JPN <- as.numeric(gdp["JPN", 2020, "gdp_SSP2EU"])
  gdp_AUS <- as.numeric(gdp["AUS", 2020, "gdp_SSP2EU"])
  gdp_MYS <- as.numeric(gdp["MYS", 2020, "gdp_SSP2EU"])

  JM <- data[data$country == "Japan-Malaysia", ]
  AJ <- data[data$country == "Australia-Japan", ]

  # remove mixed-country data and attach single country data weighted by GDP
  data <- data %>%
    filter(!country %in% c("Japan-Malaysia", "Australia-Japan")) %>%
    rbind(
    mutate(JM, country = "Japan",     value = value*gdp_JPN/(gdp_JPN+gdp_MYS)),
    mutate(JM, country = "Malaysia",  value = value*gdp_MYS/(gdp_JPN+gdp_MYS)),
    mutate(AJ, country = "Australia", value = value*gdp_AUS/(gdp_JPN+gdp_AUS)),
    mutate(AJ, country = "Japan",     value = value*gdp_JPN/(gdp_JPN+gdp_AUS))
    )

  if (subtype == "historical") {
    hist <- data %>%
      filter(.data$start <= 2023,
             .data$status != "Suspended/cancelled/decommissioned")
    # ASSUMPTION: if no end year is given, assume project remains active at
    # least until present
    hist[is.na(hist$end), "end"] <- 2023

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
      filter(.data$period <= 2023) %>%
      mutate(variable = "Carbon Management|Storage")

    cap <- cap[, c("country", "period", "variable", "value")]
    x <- as.magpie(cap, spatial = 1)

  } else if (subtype == "projections") {

    # ASSUMPTION: if empty start year, assume project start in 2030
    # end in 2030, unless decommissioned before
    proj <- data %>%
      mutate(
        "start" = ifelse(is.na(.data$start), 2030, .data$start),
        "end" = ifelse(is.na(.data$end), 2030, .data$end)
      ) %>%
    # remove entries where suspension is before operation or in same year
    filter(.data$start < .data$end)

    # ASSUMPTION: low or high expectations for realized projects by status
    statusLow <- c("Operational", "Under construction")
    statusHigh <- c(statusLow, "Planned")

    proj <- proj %>%
      filter(.data$status %in% statusHigh)

    # convert to data.frame with yearly active capacities
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

  } else if (subtype == "pipeline") {

    # ASSUMPTION: if empty start year, remove project
    # assume projects run at least until 2030
    pipe <- data %>%
      filter(!is.na(.data$start)) %>%
      mutate("end" = ifelse(is.na(.data$end), 2030, .data$end)) %>%
      # remove entries where suspension is before operation or in same year
      filter(.data$start < .data$end) %>%
      # remove cancelled projects
      filter(.data$status != "Suspended/cancelled/decommissioned")

    # convert to data.frame with yearly active capacities
    tmp <- NULL
    for (i in seq_len(nrow(pipe))) {
      tmp <- rbind(
        tmp,
        data.frame(
          pipe[i, c("country", "status", "value")],
          period = seq(as.numeric(pipe[i, "start"]), as.numeric(pipe[i, "end"]), 1)
        )
      )
    }

    # aggregate by country and period, new variables depending on status
    capOp <- aggregate(value ~ country + period,
                       data = filter(tmp, .data$status %in% "Operational"),
                       FUN = sum
    ) %>%
      mutate(status = "operational")

    capCon <- aggregate(value ~ country + period,
                        data = filter(tmp, .data$status %in% "Under construction"),
                        FUN = sum
    ) %>%
      mutate(status = "construction")

    capPlan <- aggregate(value ~ country + period,
                         data = filter(tmp, .data$status %in% "Planned"),
                         FUN = sum
    ) %>%
      mutate(status = "planned")

    cap <- rbind(capOp, capCon, capPlan) %>%
      mutate(variable = "Carbon Management|Storage")
    cap <- cap[, c("country", "period", "variable", "status", "value")]
    x <- as.magpie(cap, spatial = 1)

  } else {
    stop("Invalid subtype. Must be either
         `historical`, `pipeline` or `projections`")
  }

  x[is.na(x)] <- 0

  return(x)
}
