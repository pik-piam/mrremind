#' Read GGDC 10-Sector Database - https://www.rug.nl/ggdc/structuralchange/previous-sector-database/10-sector-2014
#' @author Renato Rodrigues
#' @examples
#'  \dontrun{
#' a <- readSource("GGDC10",convert=F)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tidyr gather
#' @importFrom dplyr %>%
#'

readGGDC10 <- function() {

  Sector <- Value <- NULL # empty declarations of variables used in dplyr operations

  data <- read_xlsx(path = "ggdc10.xlsx", sheet = "dataset")

  dataLong <- data[, -c(2, 3)] %>%
    gather(Sector, Value, -c(1:3))

  x <- as.magpie(dataLong, spatial = 1, temporal = 3, datacol = 5)

  return(x)

}
