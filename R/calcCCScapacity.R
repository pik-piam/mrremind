#' calc CCS capacity
#'
#' Calculate CCS capacity from IEA CCUS data
#'
#' @author Anne Merfort, Falk Benke
#'
#' @param subtype either `historical` for data until 2022 or `projections`
#' for projections in 2020, 2025 and 2030 (including some redistribution on EU/NEU level)
#'
#' @export
calcCCScapacity <- function(subtype) {
  x <- calcOutput("ProjectPipelines", subtype = "CCS", aggregate = F)

  if (subtype == "pipeline") {
    # used as input-data for CCS bounds
    x <- x[, c(2020, 2025, 2030), c("operational", "construction", "planned")]
    # remove "model", "variable" and "unit" dimension
    x <- collapseDim(x, keepdim = "status")
  }

  if (subtype == "historical") {
    # project pipeline snapshot from beginning of 2024
    x <- x[, seq(2005, 2023), "operational"]
    # remove "status" and "unit" dimension
    x <- collapseDim(x, keepdim = c("model", "variable"))
  }

  return(list(
    x = x,
    weight = NULL,
    unit = "MtCO2/yr",
    description = "CCS capacity derived from IEA CCUS project database"
  ))
}
