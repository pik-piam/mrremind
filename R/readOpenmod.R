#' Read Openmod capacities data
#'
#' Read-in an modified openmod capacities data file as magclass object
#'
#'
#' @return magpie object of the LIMES team updated Openmod data on capacities (GW)
#' @author Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "Openmod")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#'

readOpenmod <- function() {
  # read 2010 capacity data
  data2010 <- read.csv2("LIMES_gencap_2010.csv", stringsAsFactors = FALSE)
  data2010$period <- 2010
  # data in wide format. melt requires library(reshape2)
  data2010 <- melt(data2010, id.vars = c("dummy", "period"), variable.name = "tech", value.name = "value")
  data2010$value <- as.numeric(data2010$value)
  # read 2015 capacity data
  data2015 <- read_excel("Generation capacity EU countries 2015 v3 - in preparation.xlsx", sheet = "gencap2015_nooldT")
  data2015$period <- 2015
  # data in wide format. melt requires library(reshape2)
  data2015 <- melt(data2015, id.vars = c("dummy", "period"), variable.name = "tech", value.name = "value")
  # data
  x <- rbind(data2010, data2015)
  # transform to magpie
  x <- as.magpie(x, temporal = 2, spatial = 1, datacol = 4)

  return(x)
}
