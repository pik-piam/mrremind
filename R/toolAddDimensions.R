#' Wrapper around magclass::add_dimension supporting more than one value for the new dimension.
#' For each value, the input magclass object is copied, extended by the new dimension and
#' appended to the output.
#'
#' @param x a magclass object
#' @param dimVals list of values for the new dimension to be added
#' @param dimName name of the new dimension
#' @param dimCode dimension number of the new dimension (e.g. 3.1)
#' @returns the extended magclass object

toolAddDimensions <- function(x, dimVals, dimName, dimCode) {
  do.call("mbind", lapply(dimVals, function(item) {
    add_dimension(x, dim = dimCode, add = dimName, nm = item)
  }))
}
