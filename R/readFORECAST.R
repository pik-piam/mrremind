#' Read FORECAST data
#' @md
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr %>% mutate
#' @importFrom magclass as.magpie
#' @importFrom readxl read_xlsx

readFORECAST <- function() {

  data <- read_xlsx("~/madrat/sources/FORECAST/FORECAST v1.0_8Gt_Bal.xlsx", sheet = "data") %>%
    mutate(!!sym("Region") := "DEU")

  names(data) <- tolower(names(data))

  melt(data, id.vars = c("model", "scenario", "region", "variable", "unit"), variable.name = "period", value.name = "value") %>%
    as.magpie(spatial = 3) %>%
    return()
}
