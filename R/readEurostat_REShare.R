#' Read Eurostat Historical Renewable FE Share Data
#'
#' Read Eurostat Historical Renewable FE Share Data as magclass object
#'
#' @return magpie object of Eurostat Historical Renewable FE Share Data (share)
#' @author Felix Schreyer
#'
readEurostat_REShare <- function(subtype) {
  # read Eurostat spreadsheet
  ReShareRaw <- readxl::read_excel(
    path = "sdg_07_40_page_spreadsheet.xlsx",
    sheet = "Sheet 1",
    range = "A9:AP51",
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )

  # clean raw data
  ReShareHeader <- ReShareRaw[1, , drop = FALSE]
  ReShareWide <- ReShareRaw[-1, , drop = FALSE]
  names(ReShareWide) <- as.character(unlist(ReShareHeader[1, ], use.names = FALSE))
  names(ReShareWide)[1] <- "region"
  ReShareWide <- ReShareWide[, !is.na(names(ReShareWide)) & names(ReShareWide) != "", drop = FALSE]

  # Identify year columns
  YearColumns <- names(ReShareWide)[grepl("^[0-9]{4}$", names(ReShareWide))]

  # Map Eurostat country labels to ISO3 codes
  Iso3Map <- c(
    "Belgium" = "BEL",
    "Bulgaria" = "BGR",
    "Czechia" = "CZE",
    "Denmark" = "DNK",
    "Germany" = "DEU",
    "Estonia" = "EST",
    "Ireland" = "IRL",
    "Greece" = "GRC",
    "Spain" = "ESP",
    "France" = "FRA",
    "Croatia" = "HRV",
    "Italy" = "ITA",
    "Cyprus" = "CYP",
    "Latvia" = "LVA",
    "Lithuania" = "LTU",
    "Luxembourg" = "LUX",
    "Hungary" = "HUN",
    "Malta" = "MLT",
    "Netherlands" = "NLD",
    "Austria" = "AUT",
    "Poland" = "POL",
    "Portugal" = "PRT",
    "Romania" = "ROU",
    "Slovenia" = "SVN",
    "Slovakia" = "SVK",
    "Finland" = "FIN",
    "Sweden" = "SWE",
    "Iceland" = "ISL",
    "Norway" = "NOR",
    "Switzerland" = "CHE",
    "Albania" = "ALB",
    "Bosnia and Herzegovina" = "BIH",
    "Georgia" = "GEO",
    "Moldova" = "MDA",
    "Montenegro" = "MNE",
    "North Macedonia" = "MKD",
    "Serbia" = "SRB"
  )

  # clean data and map to correct regions
  ReShareWide <- ReShareWide %>%
    dplyr::filter(!is.na(.data$region), .data$region != "GEO (Labels)") %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(YearColumns),
        ~ {
          x <- trimws(.x)
          x[x %in% c(":", "")] <- NA_character_
          as.numeric(x)
        }
      ),
      iso3 = Iso3Map[as.character(.data$region)]
    ) %>%
    dplyr::filter(!is.na(.data$iso3)) %>%
    dplyr::select(.data$iso3, dplyr::all_of(YearColumns))

  # convert to long format
  ReShareLong <- tidyr::pivot_longer(
    ReShareWide,
    cols = dplyr::all_of(YearColumns),
    names_to = "period",
    values_to = "value"
  )
  ReShareLong$period <- as.numeric(ReShareLong$period)

  # convert to maglcass object
  as.magpie(ReShareLong, spatial = 1, temporal = 2)
}
