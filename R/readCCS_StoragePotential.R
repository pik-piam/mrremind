#' @importFrom readxl read_excel
#' 

readCCS_StoragePotential<- function() {
  data <- read_excel(path='EU_CCS_storage_capacity.xlsx')
  data <- data[,c("CountryCode", "CCS_capacity")]
  x <- as.magpie(data,spatial=1,datacol=2)
  return(x)
}
