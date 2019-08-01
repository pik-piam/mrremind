#' @importFrom magclass read.magpie

readREMMAG <- function(subtype) {
  files <- c(ghgprices="ghg_prices.mz",
             biodem="2ndgen_biodem.mz")
  file <- toolSubtypeSelect(subtype,files)
  
  return(read.magpie(file))
}