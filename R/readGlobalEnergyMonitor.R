#' Read Global Energy Monitor data
#'
#'
#' @author Rahel Mandaroux, Falk Benke
#'
#' @importFrom dplyr %>% filter mutate select
#' @importFrom readxl read_xlsx
#' @importFrom rlang sym
#'
#' @export
readGlobalEnergyMonitor <- function() {
  notebooks <- list(
    "Bioenergy" = list(
      file = "Global-Bioenergy-Power-Tracker-January-2023.xlsx",
      variable = "Cap|Electricity|Biomass",
      statusCol = "Operating Status"
    ),
    "Hydropower" = list(
      file = "Global-Hydropower-Tracker-May-2023.xlsx",
      variable = "Cap|Electricity|Hydro",
      # Hydro Power might affect 2 countries, we assign it to the first one
      countryCol = "Country 1",
      startCol = "Start Year",
      endCol = "Retired Year"
    ),
    "Nuclear" = list(
      file = "Global-Nuclear-Power-Tracker-January-2023.xlsx",
      variable = "Cap|Electricity|Nuclear",
      startCol = "Start Year",
      endCol = "Retired Year"
    ),
    "Solar" = list(
      file = "Global-Solar-Power-Tracker-May-2023.xlsx",
      variable = "Cap|Electricity|Solar"
    ),
    "Wind Offshore" = list(
      file = "Global-Wind-Power-Tracker-May-2023.xlsx",
      variable = "Cap|Electricity|Wind|Offshore",
      typeCol = "Installation Type",
      typeVals = c("offshore hard mount", "offshore mount unknown", "offshore floating")
    ),
    "Wind Onshore" = list(
      file = "Global-Wind-Power-Tracker-May-2023.xlsx",
      variable = "Cap|Electricity|Wind|Onshore",
      typeCol = "Installation Type",
      typeVals = c("onshore")
    ),
    "Coal" = list(
      file = "Global-Coal-Plant-Tracker-July-2023.xlsx",
      variable = "Cap|Coal",
      sheetName = "Units",
      projectCol = "Plant name"
    )
  )

  out <- NULL

  for (nb in notebooks) {
    cols <- c(
      "region" = ifelse(is.null(nb$countryCol), "Country", nb$countryCol),
      "project" = ifelse(is.null(nb$projectCol), "Project Name", nb$projectCol),
      "value" = "Capacity (MW)",
      "status" = ifelse(is.null(nb$statusCol), "Status", nb$statusCol),
      "start" = ifelse(is.null(nb$startCol), "Start year", nb$startCol),
      "end" = ifelse(is.null(nb$endCol), "Retired year", nb$endCol)
    )

    if (!is.null(nb$typeCol)) {
      cols <- c(cols, "type" = nb$typeCol)
    }

    data <- read_excel(
      path = nb$file,
      sheet = ifelse(is.null(nb$sheetName), "Data", nb$sheetName),
      trim_ws = TRUE, col_types = "text"
    ) %>%
      select(all_of(cols)) %>%
      mutate(
        !!sym("value") := as.numeric(!!sym("value")),
        !!sym("start") := as.numeric(!!sym("start")),
        !!sym("end") := as.numeric(!!sym("end"))
      )

    if (!is.null(nb$typeCol)) {
      data <- data %>%
        filter(
          !!sym("type") %in% nb$typeVals
        ) %>%
        select(-"type")
    }


    # read in completed projects
    completed <- data %>% filter(!is.na(!!sym("start")), !is.na(!!sym("end")))

    # read in projects with start year and no retirement, as well as a status
    # indicating probable realization, assume them running until 2050
    # rows with empty start year are ignored
    status <- c("announced", "pre-construction", "construction", "operating")
    ongoing <- data %>%
      filter(
        !is.na(!!sym("start")), is.na(!!sym("end")),
        !!sym("status") %in% status
      ) %>%
      mutate(!!sym("end") := 2050)

    production <- rbind(completed, ongoing)
    tmp <- NULL
    for (i in seq_len(nrow(production))) {
      d <- data.frame(
        production[i, c("region", "value", "project")],
        period = seq(as.numeric(production[i, "start"]), as.numeric(production[i, "end"]), 1)
      )
      tmp <- rbind(tmp, d)
    }
    cap <- aggregate(value ~ region + period, data = tmp, FUN = sum) %>%
      mutate(!!sym("variable") := nb$variable) %>%
      select("region", "period", "variable", "value")

    out <- rbind(out, cap)
  }
  x <- as.magpie(out, spatial = 1)

  # convert to GW
  x <- x / 1000
  x <- add_dimension(x, dim = 3.2, add = "unit", nm = "GW")
  x <- add_dimension(x, dim = 3.1, add = "model", nm = "Global Energy Monitor")

  return(x)
}
