#' Read EDGAR_LU
#' 
#' Read-in EDGAR_LU csv file as magclass object
#' 
#' @param subtype emission type
#' @return magpie object EDGAR_LU data
#' @author Florian Humpenoeder
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="EDGAR_LU")
#' }
#' 
readEDGAR_LU <- function(subtype="CO2") {
  if(subtype=="CO2") {
    a <- read.magpie("EDGAR_luchange_emissions_co2.csv")
    a[is.na(a)] <- 0
  }
  else if(subtype=="N2O") a <- read.magpie("EDGAR_agricultural_N2O_emissions.csv")
  else if (subtype=="CH4") a <- read.magpie("EDGAR_agricultural_CH4_emissions.csv")
  else stop("emission type does not exist in EDGAR_LU")
  return(a)
}  
