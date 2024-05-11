#' Convert UBA data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readUBA()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#'
#' @export
convertUBA <- function(x)
{

  return(add_columns(x, addnm = setdiff(getISOlist(), "DEU"), dim = 1, fill = NA))

}
