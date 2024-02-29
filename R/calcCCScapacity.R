#' calc CCS capacity
#'
#' Calculate CCS capacity from IEA CCUS data
#'
#' @author Anne Merfort, Falk Benke
#'
#' @param subtype either `historical` for data from until 2022 or `projections`
#' for projections in 2025 and 2030
#'
#' @export
calcCCScapacity <- function(subtype) {

  out <- readSource("IEA_CCUS", subtype = subtype)

  return(list(
    x = out,
    weight = NULL,
    unit = "MtCO2/yr",
    description = "CCS capacitiy derived from IEA CCUS project database"
  ))
}
