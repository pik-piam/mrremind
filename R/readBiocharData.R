#' Load data for implementation of biochar as magclass object
#'
#' Source:
#' Assumption for REMIND based on data collected in Dorndorf et al (submitted)
#'
#' @author Tabea Dorndorf
#'
readBiocharData <- function(subtype) {

  # read biochar price data assumptions
  if (subtype == "biocharPrices") {
    df <- readxl::read_xlsx(file.path("BiocharPricePaths_v01.xlsx"))

    # convert to magpie object
    m <- as.magpie(df)

    return(m)
  }

  # read short-term biochar capacities
  if (subtype == "biocharBounds") {
    df <- readxl::read_xlsx(file.path("BiocharDeploymentData_2411.xlsx"),
                            sheet = "REMINDassumptions")

    # keep only relevant entries
    df <- df %>% select(c("region", "year", "variable", "data"))

    # keep only entries that have a REMIND region code
    df <- df[grepl("^[A-Za-z]{3}$", df$region), ]

    # Convert production data to installed capacity assumption
    capacity_factor <- 0.6  # assumption based on REMIND capacity factors of 0.5-0.65
    # for those systems with energy co-production

    df <- df %>%
      mutate(data = dplyr::if_else(variable == "Production",
                                   data / capacity_factor, data)) %>%
      mutate(variable = dplyr::if_else(variable == "Production",
                                       "Installed Capacity", variable))

    # convert to magpie object
    m <- as.magpie(df)
    return(m)
  }

  stop("Not a valid subtype provided to readBiocharData")
}
