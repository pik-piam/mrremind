#' Calculate Secondary Energy variables
#' Currently this function only uses data from Ember, however other sources for SE are available and could be integrated
#' here using different `datasource` arguments.
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Pascal Weigmann
#'
#' @seealso [`calcOutput()`]
#'
#' @importFrom madrat toolGetMapping
#' @export

calcSE <- function() {

  x <- readSource("Ember")

  # map to REMIND variables
  map <- toolGetMapping("Mapping_Ember.csv", type="reportingVariables") %>%
    filter(!is.na(!!sym("REMIND_Variable")), !!sym("REMIND_Variable") != "")

  # choose only electricity generation variables
  x <- x[,,"TWh"]
  x <- toolAggregate(x, dim=3.1, rel=map, from = "Variable", to="REMIND_Variable", partrel = T, verbosity = 2)

  # convert units using factors from mapping
  for (var in getNames(x, dim=1)) {
    x[,,var] <- x[,,var] * map[map$REMIND_Variable == var, "Factor"]
  }

  # append unit to variable name and delete unit dimension
  getNames(x, dim=1) <- paste(getNames(x, dim=1), "(EJ/yr)")
  x <- collapseDim(x, dim=3.2)

  return(list(x = x, weight = NULL, unit = "EJ/yr",
         description = "Electricity generation from the yearly Ember electricity data set"))
}
