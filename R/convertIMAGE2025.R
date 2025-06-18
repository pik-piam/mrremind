#' Disaggregates IMAGE 2025 F-gas emissions data,
#' using EDGAR HFC emissions in 2005 as weights
#'
#' @return magpie object
#' @author Gabriel Abrahao
convertIMAGE2025 <- function(x) {
  w <- readSource("EDGAR", subtype = "HFC")
  w[is.na(w)] <- 0
  w <- dimSums(w, dim = 3)[, 2005, ] # sum over all HFC sources
  w <- w["ATA", , invert = TRUE] # remove Antarctica, which is not in the mapping
  w <- w + 1e-10 # avoid division by zero
  w <- collapseDim(w)

  mapping <- toolGetMapping("regionmappingIMAGE2025.csv", type = "regional", where = "mrremind")

  out <- toolAggregate(x, mapping, weight = w)
  out <- toolCountryFill(out, fill = 0) # Essentially adds Antarctica back with 0 emissions

  return(out)
}
