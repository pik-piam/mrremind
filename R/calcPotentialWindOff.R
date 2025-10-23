#' Calculate wind offshore potential
#'
#' Provides wind offshore potential data
#'
#'
#' @return wind offshore potential data and corresonding weights as a list of
#' two MAgPIE objects
#' @author Chen Chris Gong
#' @seealso \code{\link{readNREL}}, \code{\link{convertNREL}}
#' @examples
#'
#' \dontrun{
#' calcOutput("PotentialWindOff")
#' }
calcPotentialWindOff <- function() {
  # read wind offshore data
  technicalPotential <- readSource("NREL", subtype = "offshore")

  # ignore the different seabed depths classes that NREL offers
  technicalPotential <- collapseNames(technicalPotential[, , "Total"])
  # distance to shore should be "near: 5-20 nautical miles"
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
  # CG: increase wind offshore capacity factors by 25% to account for very different real-world values
  # NREL values seem underestimated, potentially partially due to assuming low turbines
  capacityFactor <- capacityFactor * 1.25

  # adapt wind offshore Potential for Germany based on study by Frauenhofer IWES (2025)
  # https://www.bsh.de/DE/THEMEN/Offshore/Meeresfachplanung/Flaechenentwicklungsplan_2025/Anlagen/Downloads_FEP2025/Adhoc_Analyse_Ertragsmodellg_24_25.pdf?__blob=publicationFile&v=3
  # They find around 240 TWh generation potential at average full load hours of 3200.
  # Add more offshore potential to grade 6 (~ 3000 FLH) for Germany, such that overall offshore potential reaches 240 TWh/yr
  # The conversion / 3.66 * 1e3 is from EJ to TWh.
  technicalPotential["DEU",,"6"] <- technicalPotential["DEU",,"6"] + (240 - dimSums(technicalPotential["DEU",,], dim=3) / 3.66 * 1e3) * 3.66 / 1e3

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
    description = "wind offshore potential",
    mixed_aggregation = TRUE
  ))
}
