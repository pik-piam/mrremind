#' @title calc Ember
#' @description prepare the yearly Ember electricity data set
#' To use only a part of the Ember data, call calcOutput("Ember", subtype = "...")
#' and convert to TW if you want to use capacities as input data to REMIND.
#'
#' @param subtype data subtype. Either "capacity", "generation" or "all"
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @seealso [`calcOutput()`]
#'
#' @author Pascal Weigmann
#'
#' @importFrom madrat toolAggregate toolGetMapping
#' @importFrom magclass collapseDim
#'
#' @export

calcEmber <- function(subtype = "all") {

  # get Ember data
  x <- readSource("Ember")

  if (subtype == "capacity") {
    # choose only capacity variables
    x <- x[, , "GW"]
    description <- "Capacities from the yearly Ember electricity data set"

  } else if (subtype == "generation") {
    # choose only generation variables
    x <- x[, , "TWh"]
    description <- "Electricity generation from the yearly Ember electricity data set"

  } else if (subtype == "all") {
    # remove all variables with % as unit, as they are not used and make data preparation more complicated
    x <- x[, , "%", invert = TRUE]
    description <- "Capacities and electricity generation from the yearly Ember electricity data set"

  } else {
    stop("Not a valid subtype!")
  }

  # load and apply mapping to chosen variables
  map <- toolGetMapping("Mapping_Ember.csv", type = "reportingVariables", where = "mappingfolder") %>%
    filter(!is.na(!!sym("REMIND_Variable")), !!sym("REMIND_Variable") != "")  # remove incomplete mapping rows

  # combine variable and unit in mapping
  map$REMIND_Variable_Unit <- paste0(map$REMIND_Variable, " (", map$REMIND_Unit, ")")

  # drop unit dimension as unit is now part of the variable name
  x <- collapseDim(x, dim = 3.2)

  # apply mapping - careful: units are changed already, values not converted yet
  x <- toolAggregate(x, dim = 3.1, rel = map, from = "Variable",
                     to = "REMIND_Variable_Unit", partrel = TRUE, verbosity = 2)

  # convert values using factors from mapping
  for (var in getNames(x, dim = 1)) {
    x[, , var] <- x[, , var] * map[map$REMIND_Variable_Unit == var, "Factor"]
  }

  return(list(x = x, weight = NULL, unit = "various", description = description))
}
