#' Read IEA CCUS data
#'
#' Reads in capacities from projects in IEA CCUS database
#'
#' @author Anne Merfort, Falk Benke
#'
#' @param subtype either `historical` for data until 2024,
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

  data <- readxl::read_excel("IEA CCUS Projects Database 2025.xlsx",
    sheet = "CCUS Projects Database"
  ) %>%
    select(
      "project" = "Project name",
      "country" = "Country or economy",
      "type" = "Project type",
      "start" = "Operation",
      "end" = "Suspension/decommissioning/cancellation",
      "status" = "Project Status",
      "value" = "Estimated capacity by IEA (Mt CO2/yr)"
    ) %>%
    # remove entries without announced capacities and not matching project type
    filter(!is.na(.data$value), .data$type %in% projectTypes)

  # correct mistakes in IEA Data
  data[data$country == "Lybia", "country"] <- "Libya"
  data[data$country == "Island", "country"] <- "Iceland"


  # new shared projects:
  # - United States-Canada,
  # - Netherlands-Belgium,
  # - Australia-Timor Leste,
  # - France-Spain,
  # - Japan-Australia

  # share capacities of projects by multiple countries according to their GDP
  gdp <- calcOutput("GDP", scenario = "SSP2", aggregate = FALSE)
  gdp_JPN <- as.numeric(gdp["JPN", 2020, ])
  gdp_AUS <- as.numeric(gdp["AUS", 2020, ])
  gdp_MYS <- as.numeric(gdp["MYS", 2020, ])
  gdp_USA <- as.numeric(gdp["USA", 2020, ])
  gdp_CAN <- as.numeric(gdp["CAN", 2020, ])
  gdp_NLD <- as.numeric(gdp["NLD", 2020, ])
  gdp_BEL <- as.numeric(gdp["BEL", 2020, ])
  gdp_TLS <- as.numeric(gdp["TLS", 2020, ])
  gdp_FRA <- as.numeric(gdp["FRA", 2020, ])
  gdp_ESP <- as.numeric(gdp["ESP", 2020, ])

  JM <- data[data$country == "Japan-Malaysia", ]
  AJ <- data[data$country %in% c("Australia-Japan", "Japan-Australia"), ]
  UC <- data[data$country == "United States-Canada", ]
  NB <- data[data$country == "Netherlands-Belgium", ]
  AT <- data[data$country == "Australia-Timor Leste", ]
  FS <- data[data$country == "France-Spain", ]

  # remove mixed-country data and attach single country data weighted by GDP
  data <- data %>%
    filter(!.data$country %in% c("Japan-Malaysia",
                                 "Australia-Japan",
                                 "Japan-Australia",
                                 "United States-Canada",
                                 "Netherlands-Belgium",
                                 "Australia-Timor Leste",
                                 "France-Spain")) %>%
    rbind(
    mutate(JM, country = "Japan",     value = .data$value*gdp_JPN/(gdp_JPN + gdp_MYS)),
    mutate(JM, country = "Malaysia",  value = .data$value*gdp_MYS/(gdp_JPN + gdp_MYS)),

    mutate(AJ, country = "Australia", value = .data$value*gdp_AUS/(gdp_JPN + gdp_AUS)),
    mutate(AJ, country = "Japan",     value = .data$value*gdp_JPN/(gdp_JPN + gdp_AUS)),

    mutate(UC, country = "United States", value = .data$value*gdp_USA/(gdp_USA + gdp_CAN)),
    mutate(UC, country = "Canada",        value = .data$value*gdp_CAN/(gdp_USA + gdp_CAN)),

    mutate(NB, country = "Netherlands", value = .data$value*gdp_NLD/(gdp_NLD + gdp_BEL)),
    mutate(NB, country = "Belgium",     value = .data$value*gdp_BEL/(gdp_NLD + gdp_BEL)),

    mutate(AT, country = "Australia",   value = .data$value*gdp_AUS/(gdp_AUS + gdp_TLS)),
    mutate(AT, country = "Timor Leste", value = .data$value*gdp_TLS/(gdp_AUS + gdp_TLS)),

    mutate(FS, country = "France", value = .data$value*gdp_FRA/(gdp_FRA + gdp_ESP)),
    mutate(FS, country = "Spain",  value = .data$value*gdp_ESP/(gdp_FRA + gdp_ESP))
    )

  if (subtype == "historical") {
    hist <- data %>%
      filter(.data$start <= 2024, .data$status != "Cancelled")
    # ASSUMPTION: if no end year is given, assume project remains active at
    # least until present
    hist[is.na(hist$end), "end"] <- 2024

    # expand data from start-end format to yearly capacity
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

    # summation of projects to get country totals
    cap <- stats::aggregate(value ~ country + period, data = tmp, FUN = sum) %>%
      filter(.data$period <= 2024) %>%
      mutate(variable = "Carbon Management|Storage")

    cap <- cap[, c("country", "period", "variable", "value")]
    x <- as.magpie(cap, spatial = 1)

  } else if (subtype == "projections") {

    # ASSUMPTION: if empty start year, assume project start in 2030
    # end in 2030, unless decommissioned before
    proj <- data %>%
      mutate(
        "start" = ifelse(is.na(.data$start), 2030, .data$start),
        "end"   = ifelse(is.na(.data$end),   2030, .data$end)
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

    capLow <- stats::aggregate(value ~ country + period,
      data = filter(tmp, .data$status %in% statusLow),
      FUN = sum
    ) %>%
      mutate(variable = "low")

    capHigh <- stats::aggregate(value ~ country + period,
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
      filter(.data$status != "Cancelled")

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
    capOp <- stats::aggregate(value ~ country + period,
                       data = filter(tmp, .data$status %in% "Operational"),
                       FUN = sum
    ) %>%
      mutate(status = "operational")

    capCon <- stats::aggregate(value ~ country + period,
                        data = filter(tmp, .data$status %in% "Under construction"),
                        FUN = sum
    ) %>%
      mutate(status = "construction")

    capPlan <- stats::aggregate(value ~ country + period,
                         data = filter(tmp, .data$status %in% "Planned"),
                         FUN = sum
    ) %>%
      mutate(status = "planned")

    cap <- rbind(capOp, capCon, capPlan) %>%
      mutate(variable = "Carbon Management|Storage")
    cap <- cap[, c("country", "period", "variable", "status", "value")]
    x <- as.magpie(cap, spatial = "country")

  } else {
    stop("Invalid subtype. Must be either
         `historical`, `pipeline` or `projections`")
  }

  x[is.na(x)] <- 0

  return(x)
}
