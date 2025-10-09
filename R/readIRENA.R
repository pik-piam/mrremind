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
  # Reading renewables electricity capacity or generation values from xlsx
  data <- readxl::read_xlsx("2025/IRENA_Statistics_Extract_2025H2.xlsx", sheet = "Country")

  if (subtype == "Capacity") {
    data <- data %>%
      mutate(value = .data$`Electricity Installed Capacity (MW)`)
  } else if (subtype == "Generation") {
    data <- data %>%
      mutate(value = .data$`Electricity Generation (GWh)`)
  } else {
    stop("Not a valid subtype!")
  }

  data <- data %>%
    select(c("Year", "ISO3 code", "RE or Non-RE", "Group Technology", "Technology",
             "Sub-Technology", "value"))

  # Technology information is split over multiple columns, assemble it and
  # calculate sums where necessary
  data <-
    rbind(data %>%
            filter(.data$`RE or Non-RE` == "Total Renewable",
                   !is.na(.data$value)) %>%
            mutate(Technology = "Total renewable energy"),
          data %>%
            filter(.data$`Group Technology` %in%
                     c("Hydropower (excl. Pumped Storage)",
                       "Wind energy", "Bioenergy", "Solar energy",
                       "Geothermal energy", "Marine energy"),
                   !is.na(.data$value)) %>%
            mutate(Technology = .data$`Group Technology`),
          data %>%
            filter(.data$`Technology` %in%
                     c("Onshore wind energy", "Offshore wind energy",
                       "Solar photovoltaic", "Liquid biofuels", "Solid biofuels",
                       "Renewable hydropower", "Biogas", "Pumped storage",
                       "Renewable municipal waste"),
                   !is.na(.data$value)) %>%
            mutate(Technology = .data$`Technology`),
          data %>%
            filter(.data$`Sub-Technology` %in%
                     c("Concentrated solar power",
                       "Other primary solid biofuels n.e.s.",
                       "Bagasse"),
                   !is.na(.data$value)) %>%
            mutate(Technology = .data$`Sub-Technology`)
          )  %>%
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
    "Other primary solid biofuels n.e.s." ~ "Other solid biofuels",
    .default = .data$Technology
  ))

  # creating capacity or generation magpie object
  x <- as.magpie(data, temporal = 1, spatial = 2, datacol = 4)
  return(x)
}
