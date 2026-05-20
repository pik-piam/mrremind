#' Aggregate regions without global sum
#'
#' @description
#' Aggregate regional data, but if a global region exists, discard the
#' automatically aggregated global values and replace them with global data.
#'
#' @param x a magclass object in country resolution
#' @param glo magclass object supplying explicit global data
#' @param rel aggregation mapping for ``toolAggregate``
#' @param to aggregation target for ``toolAggregate``
#' @param removeAllAgg decide whether to exclude all aggregated data or keep
#' those (variables/periods) that are not overwritten by data from glo object
#' @author Falk Benke, Pascal Weigmann
#' @returns magclass object
#'
#' @export

toolAggregateWithoutGlobal <- function(x, glo, rel,
                                       to = NULL,
                                       removeAllAgg = FALSE) {

  x <- toolAggregate(x, rel = rel, to = to)

  if ("GLO" %in% getItems(x, dim = 1)) {

    if (removeAllAgg) x <- x["GLO", , , invert = TRUE]

    # new magclass object that can contain country and global data
    out <- new.magpie(
      cells_and_regions = union(getItems(x, dim = 1), "GLO"),
      years = union(getYears(x), getYears(glo)),
      names = union(getNames(x), getNames(glo)),
      fill = NA,
      sets = names(dimnames(x))
    )
    out[getItems(x, dim = 1), getYears(x), getNames(x)] <- x

    # override global data with manually supplied global data
    out["GLO", getYears(glo), getNames(glo)] <- glo

    return(out)

  } else {

    return(x)
  }
}
