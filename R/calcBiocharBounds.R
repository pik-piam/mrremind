#' Provide price path data
#'
#' @author Tabea Dorndorf
#'
calcBiocharBounds <- function() {
  x <- readSource("BiocharData", subtype = "biocharBounds")

  x[is.na(x)] <- 0

  getNames(x) <- NULL

  return(list(
    x = x,
    weight = NULL,
    unit = "t BC/a",
    description = "Biochar production capacity in 2020 and 2025 by region.
    Based on data from the European Biochar Market Report 2024/25 and
    2023 Global Biochar Market Report."
  ))
}
