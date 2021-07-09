#' Read UBA
#' @md
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate
#' @importFrom rlang sym
#' @importFrom reshape2 melt
#' @importFrom magclass as.magpie
#'
#' @export
readUBA <- function() {
  
  data <- read.csv2("uba_emissions.csv", sep = ",") %>%
    melt(id.vars = c(1:2)) %>%
    select("variable" = "Sektor", "period" = "variable", "unit" = "Unit", "value") %>%
    mutate("period" := as.numeric(sub("X", "", !!sym("period"))), "region" := "DEU", "value" := as.numeric(!!sym("value"))) %>%
    select("region", "variable", "unit", "period", "value") %>%
    as.magpie() %>%
    return()
}
