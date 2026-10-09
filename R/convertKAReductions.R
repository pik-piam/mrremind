#' Convert the Kigali Amendment base tables
#'
#' Only the country-group membership ("groups") is a country-level object: it is filled to all ISO countries
#' (countries without a group get 0 in every group). The "schedule" is global and must be read with `convert = FALSE`.
#'
#' @param x magpie object from [readKAReductions()]
#' @param subtype "groups"
#' @return magpie object at country level
#' @author Gabriel Abrahao
convertKAReductions <- function(x, subtype = "groups") {
  if (subtype != "groups") stop("Only the subtype 'groups' is country-level; read '", subtype, "' with convert = FALSE")
  toolCountryFill(x, fill = 0)
}
