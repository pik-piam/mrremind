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
      "2.2", "6.1", "6.2", "6.3",
      "6.4", "6.5", "6.6"
    ),
    name = c(
      "Endenergieverbrauch",
      "Endenergieverbrauch nach Energietr\u00E4gern",
      "Endenergieverbrauch Bergbau, Gewinnung von Steinen und Erden und Verarbeitendes Gewerbe nach Energietr\u00E4gern",
      "Endenergieverbrauch Private Haushalte nach Energietr\u00E4gern",
      "Endenergieverbrauch Gewerbe, Handel, Dienstleistungen (GHD) nach Energietr\u00E4gern",
      "Endenergieverbrauch Landwirtschaft, Fischerei, Bauwirtschaft nach Energietr\u00E4gern",
      "Endenergieverbrauch Verkehr nach Energietr\u00E4gern"
    ),
    range = c(
      "A2:AF14", "A2:AF13", "A2:AF13", "A2:AF13",
      "A2:AF13", "A2:AF13", "A2:AF13"
    )
  )

  data <- NULL

  for (i in seq(1:nrow(sheets))) {
    tmp <- read_xlsx(
      path = "awt_2019.xlsx", sheet = sheets[["sheet"]][[i]], col_names = T,
      range = sheets[["range"]][[i]], .name_repair = "minimal", na = c("n/a")
    ) %>%
      filter(!is.na(!!sym("Einheit"))) %>%
      mutate(!!sym("Energietr\u00E4ger") := paste0(sheets[["name"]][[i]], "|", !!sym("Energietr\u00E4ger")))

    data <- bind_rows(data, tmp)
  }

  data <- data %>%
    melt(id.vars = c("Energietr\u00E4ger", "Einheit"), variable.name = "period", value.name = "value")

  data$region <- "DEU"

  data %>%
    select("region", variable = "Energietr\u00E4ger", unit = "Einheit", "period", "value") %>%
    as.magpie() %>%
    return()
}
