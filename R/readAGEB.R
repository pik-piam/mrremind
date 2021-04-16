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
      "Endenergieverbrauch nach Energieträgern",
      "Endenergieverbrauch Bergbau, Gewinnung von Steinen und Erden und Verarbeitendes Gewerbe nach Energieträgern",
      "Endenergieverbrauch Private Haushalte nach Energieträgern",
      "Endenergieverbrauch Gewerbe, Handel, Dienstleistungen (GHD) nach Energieträgern",
      "Endenergieverbrauch Landwirtschaft, Fischerei, Bauwirtschaft nach Energieträgern",
      "Endenergieverbrauch Verkehr nach Energieträgern"
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
      mutate(Energieträger := paste0(sheets[["name"]][[i]], "|", !!sym("Energieträger")))

    data <- bind_rows(data, tmp)
  }

  data <- data %>%
    melt(id.vars = c("Energieträger", "Einheit"), variable.name = "period", value.name = "value")

  data$region <- "DEU"

  data %>%
    select("region", variable = "Energieträger", unit = "Einheit", "period", "value") %>%
    as.magpie() %>%
    return()
}
