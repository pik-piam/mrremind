#' Read UBA
#'
#' @author Falk Benke
#'
#' @importFrom readxl read_xlsx
#' @importFrom tibble tibble
#' @importFrom dplyr select mutate
#'
readUBA <- function() {
  sheets <- tibble(
    sheet = c("THG", "CO2"),
    name = c("Emi|GHG", "Emi|CO2"),
    range = c("B4:AL54", "B4:AL54"),
    unit = c("Mt CO2-equiv/yr", "Mt CO2/yr")
  )

  data <- NULL

  for (i in seq(1:nrow(sheets))) {

    tmp <- suppressWarnings(
      read_xlsx(
        path = "datentabelle_zu_den_treibhausgas-emissionen_2024.xlsx",
        sheet = sheets[["sheet"]][[i]],
        col_names = T,
        col_types = c("text", "text", rep("numeric", 35)),
        range = sheets[["range"]][[i]],
        .name_repair = "minimal",
        na = c("n/a")
      )
    ) %>%
      select(-2)

    colnames(tmp) <- c("sektor", seq(1990, 2024, 1))

    tmp <- tmp %>%
      filter(!is.na(.data$sektor)) %>%
      mutate(
        "sektor" = paste0(sheets[["name"]][[i]], "|", sub("\\d - ", "", .data$sektor)),
        "unit" = sheets[["unit"]][[i]]
      )

    data <- bind_rows(data, tmp)
  }

  reshape2::melt(data, id.vars = c("sektor", "unit")) %>%
    select("period" = "variable", "unit" = "unit", "value", "variable" = "sektor") %>%
    mutate(
      "region" = "DEU",
      "value" = as.numeric(.data$value) / 1000
    ) %>%
    select("region", "variable", "unit", "period", "value") %>%
    filter(!is.na(!!sym("value"))) %>%
    as.magpie()
}
