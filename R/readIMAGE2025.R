#' Read F-gases emissions data from several IMAGE scenarios, obtained in 2025
#'
#' @return magpie object with several F-gas emission variables for several IMAGE scenarios
#' @author Gabriel Abrahao
#' @importFrom readxl read_excel
readIMAGE2025 <- function() {
  # Read raw data from Excel file sent by Matthias Harmsen in early 2025
  # as part of the ScenarioMIP effort
  indata <- read_excel("F_gas_data_PIK.xlsx")
  indata <- tidyr::pivot_longer(indata, 6:25, names_to = "period", values_to = "value")
  colnames(indata) <- tolower(colnames(indata))
  
  # There are several duplicated scenarios in the original data, so we
  # keep only the last one as the defaultt as.magpie behaviour, and suppress warnings
  mdata <- suppressWarnings(as.magpie(indata, spatial = "region", temporal = "period"))

  # Drop global region
  mdata <- mdata["World", , invert = TRUE]

  return(mdata)
}
