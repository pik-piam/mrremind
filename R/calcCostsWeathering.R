#' Calculate costs of transport of enhanced weathering
#'
#' @return transport costs of spreading rock on the fields
#' @seealso \code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' calcOutput("CostsWeathering")
#' }
#'
calcCostsWeathering <- function() {
  costs <- readSource("Strefler", subtype = "weathering_costs")

  # convert from $2005 to $2017

  x <- GDPuc::convertGDP(
    gdp = costs,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2017 Int$PPP",
    replace_NAs = "with_USA"
  )

  weight <- costs # get the same dimensions of the data
  weight[, , ] <- 1 # this will take the average of the countries to get the regional resolution

  return(list(
    x = x,
    weight = weight,
    unit = "T US$2017/Gt stone",
    description = "Transport costs for spreading rocks on the fields.
              The first rlf corresponds to climate regions (1 - warm, 2 - temperate).
              The second rlf are the distances of fields from the sources of the material."
  ))
}
