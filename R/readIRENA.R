#' Read IRENA
#'
#' Read-in an IRENA xlsx file as magclass object
#'
#' @param subtype data subtype. Either "Capacity" or "Generation"
#' @return magpie object of the IRENA data with historical electricity renewable
#' capacities (MW) or generation levels (GWh)
#' @author Renato Rodrigues, Pascal Weigmann
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "IRENA", subtype = "Capacity")
#' }
#'
#' @importFrom dplyr mutate rename select

readIRENA <- function(subtype) {

  # Reading renewables electricity capacity or generation values from xlsx
  data <- readxl::read_xlsx("2024/IRENA_Stats_Extract_ 2024_H1_V1.xlsx",
                            sheet = "All Data")

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
    select(c("ISO3 code", "RE or Non-RE", "Group Technology", "Technology",
             "Sub-Technology", "Year", "value"))

  # Technology information is split over multiple columns, assemble it and
  # calculate sums where necessary
  data <-
    rbind(data %>%
            filter(`RE or Non-RE` == "Total Renewable",
                   !is.na(.data$value)) %>%
            mutate(Technology = "Total renewable energy"),
          data %>%
            filter(`Group Technology` %in%
                     c("Hydropower (excl. Pumped Storage)",
                       "Wind energy", "Bioenergy", "Solar energy",
                       "Geothermal energy", "Marine energy"),
                   !is.na(.data$value)) %>%
            mutate(Technology = `Group Technology`),
          data %>%
            filter(`Technology` %in%
                     c("Onshore wind energy", "Offshore wind energy",
                       "Solar photovoltaic", "Liquid biofuels", "Solid biofuels",
                       "Renewable hydropower", "Biogas", "Pumped storage",
                       "Renewable municipal waste"),
                   !is.na(.data$value)) %>%
            mutate(Technology = `Technology`),
          data %>%
            filter(`Sub-Technology` %in%
                     c("Concentrated solar power",
                       "Other primary solid biofuels n.e.s.",
                       "Bagasse"),
                   !is.na(.data$value)) %>%
            mutate(Technology = `Sub-Technology`)
          )  %>%
    group_by(.data$`ISO3 code`, .data$Year, .data$Technology) %>%
    summarise(value = sum(.data$value), .groups = "drop")

  # harmonize Technology names with older version
  mask <- data.frame(
    tech_before = c("Hydropower (excl. Pumped Storage)",
                    "Wind energy",
                    "Solar energy",
                    "Geothermal energy",
                    "Marine energy",
                    "Other primary solid biofuels n.e.s."),
    tech_after = c("Hydropower",
                   "Wind",
                   "Solar",
                   "Geothermal",
                   "Marine",
                   "Other solid biofuels"))

  for (n in 1:nrow(mask)) {
    data[data$Technology == mask[n, 1], "Technology"] <- mask[n, 2]
  }

  # rearrange column order to more readable format:
  # year, country, tech, value (capacity or generation)
  data <- data[, c(2, 1, 3, 4)]

  # fix name of regional column to the value it used to have before 9678353
  data <- data %>%
    rename(`Country/area` = 2)

  # creating capacity or generation magpie object
  x <- as.magpie(data, temporal = 1, spatial = 2, datacol = 4)
  return(x)
}