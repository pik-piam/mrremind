#' Calculate costs of transport of enahnced weathering
#'
#' @return transport costs of spreading rock on the fields
#' @seealso \code{\link{calcOutput}}
#' @examples
#'
#' \dontrun{
#' calcOutput("CostsWeathering")
#' }
#'

calcCostsWeathering <- function() {

  costs <- readSource("Strefler", subtype = "weathering_costs")
  weight <- costs # get the same dimensions of the data
  weight[, , ] <- 1 # this will take the average of the countries to get the regional resolution

  return(list(x = costs,
              weight = weight,
              unit = "T$/Gt stone",
              description = "Transport costs for spreading rocks on the fields. The first rlf corresponds to climate regions (1 - warm, 2 - temperate). The second rlf are the distances of fields from the sources of the material."
  ))
}