#' Load Exclusive Economic Zone (EEZ) size data as magclass object.
#' Areas with overlapping claims or jointly governed areas are split equally
#' between the respective countries.
#'
#' @author Tabea Dorndorf
#'
readMarineRegionsOrg <- function() {
  df <- readxl::read_xlsx(file.path("v10", "eez_v12.xlsx"))

  # Select "ISO_SOV" instead of "ISO_TER". ISO_SOV assigns territories
  # to their sovereign body (e.g. Alaska to USA, Cook Islands to NZL).
  # Some territories do not have an ISO code. Chosing ISO_TER would thus reduce the
  # area taken into consideration.
  df <- df %>%
    select(c("POL_TYPE", "ISO_SOV1", "ISO_SOV2", "ISO_SOV3", "AREA_KM2"))

  # data adjustment: split areas between regions with joint regime or overlapping claims
  df <- df %>%
    # determine number of countries with claims for each area entry
    mutate(claims = dplyr::case_when(
      !is.na(ISO_SOV3) ~ 3,
      !is.na(ISO_SOV2) ~ 2,
      TRUE ~ 1
    )) %>%
    # adjust AREA_KM2 assigned to each country based on number of countries with claims
    mutate(AREA_KM2_adj = .data$AREA_KM2 * (1 / .data$claims)) %>%
    # convert wide to long format
    tidyr::pivot_longer(
      cols = c("ISO_SOV1", "ISO_SOV2", "ISO_SOV3"),
      names_to = "ISO_SOV_original",
      values_to = "ISO_SOV"
    ) %>%
    # drop NAs
    filter(!is.na(.data$ISO_SOV))

  # aggregate to one value per country
  df <- df %>%
    select("Country" = "ISO_SOV", "Value" = "AREA_KM2_adj") %>%
    dplyr::group_by(.data$Country) %>%
    summarise(Value = sum(.data$Value))

  # convert to magpie object
  m <- as.magpie(df)

  return(m)
}
