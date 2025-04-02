#' Calculate wind onshore potential
#'
#' Provides wind onshore potential data
#'
#'
#' @return wind onshore potential data and corresonding weights as a list of
#' two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readNREL}},
#' \code{\link{convertNREL}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{
#' calcOutput("PotentialWindOn")
#' 
#' }
calcPotentialWindOn <- function() {
  # read wind onshore data
  technicalPotential <- readSource("NREL", subtype = "onshore")

  # distance to load should be "near: 0-50 miles"
  technicalPotential <- collapseNames(technicalPotential[, , "near"])

  # delete total and convert from PWh to EJ
  technicalPotential <- technicalPotential[, , "total", invert = TRUE]  * 3.6
  # allocate c1-c9 to the right grades
  for (i in 1:9) {
    getNames(technicalPotential) <- gsub(paste0("c", i), 10 - i, getNames(technicalPotential))
  }

  # add "nur" data representing the capacity factor of each grade
  capacityFactor <- new.magpie(getRegions(technicalPotential), getYears(technicalPotential), getNames(technicalPotential))
  capacityFactor[, , "9"] <- 0.09
  capacityFactor[, , "8"] <- 0.20
  capacityFactor[, , "7"] <- 0.24
  capacityFactor[, , "6"] <- 0.28
  capacityFactor[, , "5"] <- 0.32
  capacityFactor[, , "4"] <- 0.36
  capacityFactor[, , "3"] <- 0.40
  capacityFactor[, , "2"] <- 0.44
  capacityFactor[, , "1"] <- 0.48

  # put technicalPotential (maxprod) and capacityFactor (nur) together
  technicalPotential <- add_dimension(technicalPotential, dim = 3.1, add = "char", nm = "maxprod")
  capacityFactor <- add_dimension(capacityFactor, dim = 3.1, add = "char", nm = "nur")
  data <- mbind(technicalPotential, capacityFactor)

  # create weight-matrix
  w <- new.magpie(getRegions(data), getYears(data), getNames(data), fill = 1)
  w[, , "maxprod"] <- NA

  return(list(
    x = data,
    weight = w,
    unit = "EJ/a",
    description = "wind onshore potential",
    mixed_aggregation = TRUE
  ))
}
