#' Read AGEB
#' @md
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom readxl read_xlsx
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows select filter mutate
#' @importFrom rlang sym
#' @importFrom reshape2 melt
#' @importFrom magclass as.magpie
#'
#' @export
readAGEB <- function() {
  sheets <- tibble(
    sheet = c(
      "1.1", "1.2", "1.3", "1.4", "1.5",
      "2.1", "2.2",
      "3.1",
      "4.1", "4.2",
      "6.1", "6.2", "6.3", "6.4", "6.5", "6.6"
    ),
    name = c(
      "1.1 Primaerenergiegewinnung im Inland nach Energietraegern",
      "1.2 Einfuhr",
      "1.3 Ausfuhr",
      "1.4 Nettoeinfuhr",
      "1.5 Bunkerungen seegehender Schiffe",
      "2.1 Primaerenergieverbrauch nach Energietraegern",
      "2.2 Struktur des Energieverbrauchs nach Sektoren",
      "3.1 Primaerenergieverbrauch erneuerbare Energien",
      "4.1 Einsatz von Energietraegern zur Stromerzeugung",
      "4.2 Einsatz von Energietraegern zur Fernwaermeerzeugung",
      "6.1 Endenergieverbrauch nach Energietraegern",
      "6.2 Endenergieverbrauch Bergbau, Gewinnung von Steinen und Erden und Verarbeitendes Gewerbe nach Energietraegern",
      "6.3 Endenergieverbrauch Private Haushalte nach Energietraegern",
      "6.4 Endenergieverbrauch Gewerbe, Handel, Dienstleistungen (GHD) nach Energietraegern",
      "6.5 Endenergieverbrauch Landwirtschaft, Fischerei, Bauwirtschaft nach Energietraegern",
      "6.6 Endenergieverbrauch Verkehr nach Energietraegern"
    ),
    range = c(
      "B4:AH13", "B4:AH15", "B4:AH15", "B4:AH15", "B4:AH10",
      "B4:AH15", "B4:AH16",
      "B4:AH15",
      "B4:AH15", "B4:AH15",
      "B4:AH15", "B4:AH15", "B4:AH15", "B4:AH15", "B4:AH15", "B4:AH15"
    )
  )

  data <- NULL

  for (i in seq(1:nrow(sheets))) {
    tmp <- suppressWarnings(
      read_xlsx(
        path = "awt_2020_d.xlsx", sheet = sheets[["sheet"]][[i]], col_names = TRUE,
        col_types = c("text", "text", rep("numeric", 31)),
        range = sheets[["range"]][[i]], .name_repair = "minimal", na = c("n/a")
      )
    ) %>%
      filter(!is.na(!!sym("Einheit"))) %>%
      mutate(!!sym("Energietraeger") := paste0(sheets[["name"]][[i]], "|", !!sym("Energietr\u00E4ger"))) %>%
      select(-1)

    data <- bind_rows(data, tmp)
  }

  data <- data %>%
    melt(id.vars = c("Energietraeger", "Einheit"), variable.name = "period", value.name = "value")

  data$region <- "DEU"

  data %>%
    select("region", variable = "Energietraeger", unit = "Einheit", "period", "value") %>%
    as.magpie() %>%
    return()
}
