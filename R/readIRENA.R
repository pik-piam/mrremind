#' Read IRENA
#'
#' Read-in an IRENA xlsx file as magclass object
#'
#' @param subtype data subtype. Either "Capacity" or "Generation"
#' @return magpie object of the IRENA data with historical electricity renewable
#' capacities (MW) or generation levels (GWh)
#' @author Renato Rodrigues, Pascal Weigmann
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "IRENA", subtype = "Capacity")
#' }
#'
#' @importFrom dplyr mutate rename select case_match relocate
readIRENA <- function(subtype) {
  # generation values are not (yet?) in 2026 data
  if (subtype == "Capacity") {
    fileName <- "2026/IRENA_Stats_extract_2026 H1.xlsx"
    valueColumn <- "Electricity Installed Capacity (MW)"
  } else if (subtype == "Generation") {
    fileName <- "2025/IRENA_Statistics_Extract_2025H2.xlsx"
    valueColumn <- "Electricity Generation (GWh)"
  } else {
    stop("Not a valid subtype!")
  }
  data <- readxl::read_xlsx(fileName, sheet = "Country", .name_repair = "minimal")

  if (subtype == "Capacity") {
    # columns are swapped in 2026 data
    data[c("Technology", "Sub-Technology")] = data[c("Sub-Technology", "Technology")]
    # some technologies were renamed
    data[["Group Technology"]][data[["Group Technology"]] == "Renewable hydropower (excl. Pumped Storage)"] <-
      "Hydropower (excl. Pumped Storage)"
  }


  data <- data %>%
    mutate(value = .data[[valueColumn]]) %>%
    select(c("Year", "ISO3 code", "RE or Non-RE", "Group Technology", "Technology",
             "Sub-Technology", "value"))
  # Each row contains one piece of data.
  # Which tech/sum it belongs to is stored over multiple columns, depending on the tech.
  # For each column filter all the relevant rows. Then rbind all the results for each column.
  data <- rbind(
    # total renewable capacity is stored in this type of row
    data %>%
      filter(.data$`RE or Non-RE` == "Total Renewable", !is.na(.data$value)) %>%
      mutate(Technology = "Total renewable energy"),
    # some technologies are stored in this type of row
    data %>%
      filter(.data$`Group Technology` %in%
        c(
          "Hydropower (excl. Pumped Storage)",
          "Wind energy", "Bioenergy", "Solar energy",
          "Geothermal energy", "Marine energy"
        ), !is.na(.data$value)) %>%
      mutate(Technology = .data$`Group Technology`),
    # some technologies are stored in this type of row
    data %>%
      filter(.data$`Technology` %in%
        c(
          "Onshore wind energy", "Offshore wind energy",
          "Solar photovoltaic", "Liquid biofuels", "Solid biofuels",
          "Renewable hydropower", "Biogas", "Gas biofuels"
        ), !is.na(.data$value)) %>%
      mutate(Technology = .data$`Technology`),
    # yet more technologies are stored in this type of row
    data %>%
      filter(.data$`Sub-Technology` %in%
        c(
          "Concentrated solar power",
          "Other primary solid biofuels n.e.s.",
          "Bagasse", "Pumped storage", "Renewable municipal waste"
        ), !is.na(.data$value)) %>%
      mutate(Technology = .data$`Sub-Technology`)
  ) %>%
    group_by(.data$`ISO3 code`, .data$Year, .data$Technology) %>%
    summarise(value = sum(.data$value), .groups = "drop") %>%
    relocate("Year") %>% # put Year as the first column
    rename(`Country/area` = "ISO3 code") # keep regional column name of before 9678353

  # harmonize Technology names with older version
  data <- data %>%
    mutate(Technology = case_match(.data$Technology,
      # "Hydropower" contains renewable hydropower and mixed hydro plants, but not pure pumped storage
      "Hydropower (excl. Pumped Storage)"   ~ "Hydropower",
      "Wind energy"                         ~ "Wind",
      "Solar energy"                        ~ "Solar",
      "Geothermal energy"                   ~ "Geothermal",
      "Marine energy"                       ~ "Marine",
      "Gas biofuels"                        ~ "Biogas",
      "Other primary solid biofuels n.e.s." ~ "Other solid biofuels",
      .default = .data$Technology
    ))

  # creating capacity or generation magpie object
  x <- as.magpie(data, temporal = 1, spatial = 2, datacol = 4)
  return(x)
}
