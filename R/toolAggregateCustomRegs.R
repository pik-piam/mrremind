#' Aggregate regions without regional aggregations such as global sum
#'
#' @description
#' Aggregate regional data, but if regional aggregations exist, discard the
#' automatically aggregated values and replace them with source data.
#'
#' @param x a magclass object in country resolution
#' @param agg magclass object supplying explicit regional aggregates
#' @param rel aggregation mapping for ``toolAggregate``
#' @param to aggregation target for ``toolAggregate``
#' @param removeAllAgg decide whether to exclude all aggregated data or keep
#' those (variables/periods) that are not overwritten by data from agg object
#' @param regs one or multiple names of aggregated regions to be removed/overwritten
#'
#' @author Falk Benke, Pascal Weigmann
#' @returns magclass object
#'
#' @export

toolAggregateCustomRegs <- function(x, agg, rel,
                                    to = NULL,
                                    removeAllAgg = TRUE,
                                    regs = "GLO") {

  x <- toolAggregate(x, rel = rel, to = to)

  if (any(regs %in% getItems(x, dim = 1))) {

    # elements of regs that were created by toolAggregate
    r <- intersect(regs, getItems(x, dim = 1))
    if (removeAllAgg) x <- x[r, , , invert = TRUE]

    # new magclass object that can contain country and global data
    out <- new.magpie(
      cells_and_regions = union(getItems(x, dim = 1), r),
      years = union(getYears(x), getYears(agg)),
      names = union(getNames(x), getNames(agg)),
      fill = NA,
      sets = names(dimnames(x))
    )
    out[getItems(x, dim = 1), getYears(x), getNames(x)] <- x

    # override global data with manually supplied global data
    r_agg <- intersect(regs, getItems(agg, dim = 1))
    out[r_agg, getYears(agg), getNames(agg)] <- agg

    return(out)

  } else {

    return(x)
  }
}
