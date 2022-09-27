#' Read AGEB
#' @md
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @param subtype data subtype. Either "balances" ("Auswertungstabellen zur Energiebilanz Deutschland") or "electricity" ("Bruttostromerzeugung in Deutschland nach Energieträgern")
#' @importFrom readxl read_xlsx
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows select filter mutate
#' @importFrom rlang sym
#' @importFrom reshape2 melt
#' @importFrom magclass as.magpie
#'
#' @export
readAGEB <- function(subtype = "balances") {
  if (!subtype %in% c("balances", "electricity")) {
    stop("Invalid subtype. Must be either balances or electricity")
  }

  if (subtype == "balances") {
    sheets <- tibble(
      sheet = c(
        "1.1", "1.2", "1.3", "1.4", "1.5",
        "2.1", "2.2",
        "3.1",
        "4.1", "4.2",
        "6.1", "6.2", "6.3", "6.4", "6.5", "6.6", "6.8"
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
        "6.6 Endenergieverbrauch Verkehr nach Energietraegern",
        "6.8 Endenergieverbrauch im Subsektor Strassenverkehr nach Energietraegern"
      ),
      range = c(
        "B4:AH13", "B4:AH15", "B4:AH15", "B4:AH15", "B4:AH10",
        "B4:AH15", "B4:AH16",
        "B4:AH15",
        "B4:AH15", "B4:AH15",
        "B4:AH15", "B4:AH15", "B4:AH15", "B4:AH15", "B4:AH15", "B4:AH15", "B4:AH12"
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
      reshape2::melt(id.vars = c("Energietraeger", "Einheit"), variable.name = "period", value.name = "value")

    data$region <- "DEU"

    data %>%
      select("region", variable = "Energietraeger", unit = "Einheit", "period", "value") %>%
      as.magpie() %>%
      return()
  } else if (subtype == "electricity") {
    read_xlsx(
      path = "STRERZ_2021Feb22_web.xlsx", sheet = "STRERZ (brutto)", col_names = TRUE,
      col_types = c("text", rep("numeric", 32)),
      range = "B3:AH17", .name_repair = "minimal", na = c("k.A.")
    ) %>%
      mutate(!!sym("TWh") := gsub(", darunter:", "", !!sym("TWh"))) %>%
      mutate(!!sym("TWh") := gsub("- ", "", !!sym("TWh"))) %>%
      mutate(!!sym("TWh") := gsub("[0-9])", "", !!sym("TWh"))) %>%
      mutate(!!sym("variable") := paste0("9 Bruttostromerzeugung|", !!sym("TWh"))) %>%
      select(-1) %>%
      reshape2::melt(id.vars = c("variable"), variable.name = "period", value.name = "value") %>%
      mutate(!!sym("region") := "DEU", !!sym("unit") := "TWh") %>%
      select("region", "period", "variable", "unit", "value") %>%
      as.magpie() %>%
      return()
  }
}
