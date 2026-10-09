#' Read the Kigali Amendment (KA) base tables
#'
#' Reads the two base tables of the source folder KAReductions (see README.txt there for sources, baseline years and
#' caveats). No fractions are stored in the source: they are computed by [calcKAReductionFractions()].
#'
#' @param subtype "groups": KAGroups_ISO.csv, the Kigali group of each country, returned as a 0/1 membership indicator
#' with dimensions country x group (A5G1, A5G2, nonA5, nonA5_special).
#' "schedule": KASchedule.csv, the phase-down steps of Table 1 of the UNEP Kigali FAQ, returned as a plain
#' data.frame with the columns group, step, year and reduction_pct (cumulative reduction in percent of the baseline,
#' in force from `year` until the next step of the group).
#' The schedule is not a magpie object (returned as list(x, class = "data.frame"), which madrat unpacks):
#' read it with `convert = FALSE`.
#' @return magpie object (subtype "groups") or data.frame (subtype "schedule")
#' @author Gabriel Abrahao
#' @seealso [madrat::readSource()], [calcKAReductionFractions()]
readKAReductions <- function(subtype = "groups") {
  if (subtype == "groups") {
    d <- read.csv("KAGroups_ISO.csv", stringsAsFactors = FALSE)
    # Every row of the file is a country that is a member of a group. as.magpie leaves the combinations of country and
    # group that are not in the file empty (NA), and those are set to 0 (not a member) below
    d$value <- 1
    x <- as.magpie(d[, c("iso", "group", "value")], spatial = "iso", temporal = NULL)
    x[is.na(x)] <- 0
  } else if (subtype == "schedule") {
    # madrat accepts non-magpie data from a read function as a list with the entries x and class
    x <- list(x = read.csv("KASchedule.csv", stringsAsFactors = FALSE), class = "data.frame")
  } else {
    stop("Invalid subtype. Please use 'groups' or 'schedule'")
  }
  return(x)
}
