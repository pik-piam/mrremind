#' Sets values for 6 EU countries not belonging to EU 28 but EU 34 to zero if
#' they are NA. Used to avoid EUR region not yielding NA because of these countries.
#'
#' @param x magpie object with 249 ISO country codes in the spatial dimension
#' @author Falk Benke
#' @export

toolFillEU34Countries <- function(x) {
  non28EUcountries <- c("ALA", "FRO", "GIB", "GGY", "IMN", "JEY")
  tmp <- x[non28EUcountries, , ]
  tmp[is.na(tmp)] <- 0
  x[non28EUcountries, , ] <- tmp[non28EUcountries, , ]
  return(x)
}
