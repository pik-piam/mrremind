#' Convert geological storage potential
#'
#' @return A magpie object with geological storage potential
#' @author David Klein
#'
convertGidden2025_geological_storage_potential <- function(x) {

  # What about regions that are listed in the source as "non mapped countries" among which
  # - ATA (Antarctica) is known to madrat, but might not be available for CCS
  # - XCA is neither explained in the source nor known to madrat, but has a non-zero potential

  x <- toolCountryFill(x, fill = 0, verbosity = 0) # fill missing countries

  x <- x * 12/44 # from GtCO2 to GtC

  # regions that were added by toolCountryFill
  # added <- x |> as.quitte() |> filter(is.na(value)) |> pull(region) |> unique() |> as.character()
  # none of them are considered 'important'
  # match(added, getISOlist("important"))

  return(x)

}



