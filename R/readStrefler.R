#' Get data on enhanced weathering
#'
#' @param subtype type of data, one of "weathering_graderegi", "weathering_costs"
#' @return magpie object of region dependent data
#'
#' @examples
#'
#' \dontrun{
#' a <- readSource(type="Strefler", subtype="weathering_graderegi")
#' }

readStrefler <- function(subtype) {

  #  weathering potential
  if (subtype == "weathering_graderegi") {
    x <- read.csv("f33_data_weathering_graderegi.csv", sep = ";", header = TRUE, row.names = 1)
    x <- as.magpie(x, spatial = 1)
    getSets(x)[1] <- "region"
    getSets(x)[2] <- "year"
    getSets(x)[3] <- "data1"
    return(x)
  }

  # transport costs of spreading rock for weathering on the fields
  if (subtype == "weathering_costs") {
    x <- read.csv("p33_weathering_transport_costs.csv", sep = ";", header = TRUE)
    x <- as.magpie(x, tidy = "true")
    return(x)
  }

  stop("Not a valid subtype provided to readStrefler")
}
