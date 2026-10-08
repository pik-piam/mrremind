#' Reads excel sheet of IEA hydrogen project pipeline and builds yearly capacity by country from it.
#' The data is distinguished by technology
#' @return  magpie object with Hydrogen capacity data both in MWel and in kT H2/y
#' @author  Simon Krogmann
#' @importFrom dplyr filter mutate select rowwise if_else
#' @importFrom tidyr unnest
#' @importFrom readxl read_xlsx
readIEA_HydrogenProduction <- function() {
  relevantStatus <- c("Operational", "Decommisioned", "DEMO")
  currentFullYear <- 2025
  projects <- suppressWarnings(readxl::read_excel(
    "Hydrogen Production Projects Database - June 2026.xlsx",
    sheet = "Hydrogen production projects",
    skip = 1,
    .name_repair = "minimal"
  )) %>%
    select(
      "project" = "Project name",
      "country" = "Country\r\n(ISO-3)",
      "tech" = "Technology",
      "start" = "Date online",
      "end" = "Decomission date",
      "status" = "Status",
      "electric" = "Capacity (MWel)",
      "h2" = "Capacity\r\n(kt H2/y)",
    ) %>%
    filter(.data[["status"]] %in% relevantStatus)

  confidential <- projects %>% filter(.data[["project"]] == "Other projects from confidential sources (2000-2026)")
  projects <- projects %>% filter(!is.na(.data[["start"]]), !is.na(.data[["country"]]))

  minYear <- min(projects[["start"]], na.rm = TRUE)
  years <- seq(minYear, currentFullYear)

  # expand each project to its active years
  projectYears <- projects %>%
    mutate(end_year = if_else(is.na(.data[["end"]]), currentFullYear, .data[["end"]] - 1)) %>%
    rowwise() %>%
    mutate(year = list(seq(.data[["start"]], .data[["end_year"]]))) %>%
    unnest(cols = c("year")) %>%
    filter(.data[["year"]] %in% years)

  # summarize capacity by country, year and tech
  capacitySummary <- projectYears %>%
    group_by(.data[["country"]], .data[["year"]], .data[["tech"]]) %>%
    summarise(
      electric = sum(.data[["electric"]], na.rm = TRUE),
      h2 = sum(.data[["h2"]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(
      country = unique(projects[["country"]]),
      year = years,
      tech = unique(projects[["tech"]]),
      fill = list(electric = 0, h2 = 0)
    ) %>%
    pivot_longer(
      cols = c("electric", "h2"),
      names_to = "variable",
      values_to = "value"
    )
  out <- as.magpie(capacitySummary, spatial = "country", temporal = "year")

  sums <- dimSums(out[, currentFullYear, ], dim = 1)
  additions <- confidential %>%
    pivot_longer(
      cols = c("electric", "h2"),
      names_to = "variable",
      values_to = "value"
    ) %>%
    select("tech", "variable", "value") %>%
    as.magpie()
  additions <- magclass::matchDim(additions, sums, dim = 3)
  # scale all values to match confidential projects, setYears to remove name of current year
  factor <- setYears((sums + additions) / sums)
  new <- out * factor
  out[!is.na(new)] <- new[!is.na(new)]
  return(out)
}
