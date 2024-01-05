# add documentation
toolAddDimensions <- function(x, dimVals, dimName, dimCode) {
  do.call("mbind", lapply(dimVals, function(item) {
    add_dimension(x, dim = dimCode, add = dimName, nm = item)
  }))
}
