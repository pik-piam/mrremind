#' Calculates historical emissions needed for estimating emission factors
#' in extra emissions reporting
#'
#' @param source either "PBL_2007" or "PBL_2022"
#'
#'
#' @return list of magclass with REMIND input data for
#' different sectors for timesteps 2000-2100, in Mt N per year
#' @author Gabriel Abrhahao
# nolint start
calcEmissions4ReportExtra <- function() {
  incedsall <- readSource("CEDS2024")
  # Keep just the gases we're currently interested in, but also keep all sectors
  out <- incedsall[,2020,c("ch4","n2o_n")]

  return(list(x = out, weight = NULL, unit = "Mt N or Mt CH4", description = "CEDS sectors emissions in 2020 for extra reporting"))
}
# nolint end
