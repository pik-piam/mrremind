#' Reads shares of world manufacture for spv modules and wind turbines.
#' @author Aman Malik
#' @importFrom dplyr bind_rows
#' @importFrom readxl read_excel

readProdShares <- function() {

  input_spv <- read_excel("shares.xlsx", sheet = "spv")
  input_spv$tech <- "spv"

  input_wind <- read_excel("shares.xlsx", sheet = "wind")
  input_wind$tech <- "wind"

  input <- bind_rows(input_spv, input_wind)

  x <- as.magpie(input, spatial = 1)

  return(x)
}
