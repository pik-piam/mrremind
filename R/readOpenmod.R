#' Read Openmod capacities data
#'
#' Read-in an modified openmod capacities data file as magclass object
#'
#' @return magpie object of the LIMES team updated Openmod data on capacities (GW)
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "Openmod")
#' }
#'
readOpenmod <- function() {
  # Read 2010 capacity data
  data2010 <- utils::read.csv2("LIMES_gencap_2010.csv", stringsAsFactors = FALSE)
  data2010$period <- 2010
  data2010 <- reshape2::melt(data2010, id.vars = c("dummy", "period"), variable.name = "tech", value.name = "value")
  data2010$value <- as.numeric(data2010$value)

  # Read 2015 capacity data
  data2015 <- readxl::read_excel("Generation capacity EU countries 2015 v3 - in preparation.xlsx",
                                 sheet = "gencap2015_nooldT")
  data2015$period <- 2015
  data2015 <- reshape2::melt(data2015, id.vars = c("dummy", "period"), variable.name = "tech", value.name = "value")

  x <- rbind(data2010, data2015)

  as.magpie(x, temporal = 2, spatial = 1, datacol = 4)
}
