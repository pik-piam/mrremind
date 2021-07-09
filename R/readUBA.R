#' Read UBA
#' @md
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom readxl read_xlsx
#' @importFrom tibble tibble
#' @importFrom dplyr select mutate
#' @importFrom rlang sym
#' @importFrom reshape2 melt
#' @importFrom magclass as.magpie
#'
#' @export
readUBA <- function() {
  
  sheets <- tibble(
    sheet = c("THG", "CO2"),
    name = c("Emi|GHG", "Emi|CO2"),
    range = c("B4:AH50", "B4:AH50"),
    unit = c("Mt CO2-equiv/yr", "Mt CO2/yr")
  )
  
  data <- NULL
  
  for (i in seq(1:nrow(sheets))) {
    tmp <- read_xlsx(
      path = "2021_03_10_trendtabellen_thg_nach_sektoren_v1.0.xlsx", sheet = sheets[["sheet"]][[i]], col_names = T,
      range = sheets[["range"]][[i]], .name_repair = "minimal", na = c("n/a")
    ) %>% select(-2)
    
    colnames(tmp) <- c("sektor", seq(1990,2020,1))
    
    tmp <- filter(tmp, !is.na(!!sym("sektor"))) %>%
      mutate(!!sym("sektor") := paste0(sheets[["name"]][[i]], "|", sub("\\d - ", "", !!sym("sektor"))), !!sym("unit") := sheets[["unit"]][[i]])
    
    data <- bind_rows(data, tmp)
  }
  
  melt(data, id.vars = c("sektor", "unit"))  %>%
    select("period" = "variable", "unit" = "unit", "value", "variable" = "sektor")  %>%
    mutate("region" := "DEU", "value" := as.numeric(!!sym("value")) / 1000) %>%
    select("region", "variable", "unit", "period", "value")  %>%
    filter(!is.na(!!sym("value"))) %>%
    as.magpie() %>%
    return()
}
