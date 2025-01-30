#' Read GGDC 10-Sector Database - https://www.rug.nl/ggdc/structuralchange/previous-sector-database/10-sector-2014
#' @author Renato Rodrigues
#' @examples
#'  \dontrun{
#' a <- readSource("GGDC10",convert=F)
#' }
#'
readGGDC10 <- function() {
  data <- suppressWarnings(readxl::read_xlsx(path = "ggdc10.xlsx", sheet = "dataset"))
  dataLong <- data[, -c(2, 3)] %>% tidyr::gather("Sector", "Value", -c(1:3))
  as.magpie(dataLong, spatial = 1, temporal = 3, datacol = 5)
}
