#' @title convertStrefler
#' @description Converts data on enhanced weathering
#' @param x unconverted magpie object from read-script
#' @param subtype data subtype. Either "weathering_graderegi", or "weathering_costs"
#'
#' @return magpie object with a completed dataset
#'
convertStrefler <- function(x, subtype) {

  if (subtype == "weathering_graderegi") {
    w <- calcOutput("FAOLand", aggregate = FALSE)[, , "6620", pmatch = TRUE][, 2005, ]
    y <- toolAggregate(x, "regionmappingGEC.csv", weight = w)
    return(y)
  }

  if (subtype == "weathering_costs") {
    reg_rel <- toolGetMapping("regionmappingH12.csv", type = "regional",
                              where = "mappingfolder") # get H12 regionmapping
    # assign the same cost for each country in a given region
    y <- toolAggregate(x, reg_rel)
    return(y)
  }

  stop("Not a valid subtype in convertStrefler")
}
