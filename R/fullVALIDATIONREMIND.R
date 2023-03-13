#' fullVALIDATIONREMIND
#'
#' Function that generates the historical regional dataset against which the
#' REMIND model results can be compared.
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @author David Klein
#' @seealso
#' \code{\link{fullREMIND}},\code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' fullVALIDATIONREMIND()
#' }
#'
fullVALIDATIONREMIND <- function(rev = 0) {

  #-------------- warn if there are duplicate column names --------------------------------------------

  # read region mappings
  mappings <- c(getConfig("regionmapping"), getConfig("extramappings"))
  rel <- NULL
  for (r in seq_len(length(mappings))) {
    # read mapping file
    new <- colnames(toolGetMapping(mappings[r], type = "regional"))
    # ignore "X" and "CountryCode"
    new <- new[!new %in% c("X", "CountryCode")]
    # check if columns of current mapping exist in any mapping read before
    if (any(new %in% rel)) warning("The following column(s) from ", mappings[r],
                                   " exist in another mapping an will be ignored: ",
                                   paste(new[new %in% rel], collapse = ", "))
    # append new columns removing duplicates
    rel <- unique(c(rel, new))
  }

  #-------------- use columns from all regionmappings for aggregation ---------------------------------

  columnsForAggregation <- gsub("RegionCode", "region", paste0(c(rel, "global"), collapse = "+"))

  #-------------- historical data ---------------------------------------------------------------------

  valfile <- "historical.mif"

  calcOutput("Historical", round = 5,  file = valfile, aggregate = columnsForAggregation,
             append = FALSE, na_warning = FALSE, try = TRUE)

  calcOutput(type = "IEA_ETP", aggregate = "global", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, isValidation = TRUE)

  calcOutput(type = "IEA_ETP", aggregate = "region", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, isValidation = TRUE)

  calcOutput(type = "IEA_WEO_2021", aggregate = "global", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, isValidation = TRUE)

  calcOutput(type = "IEA_WEO_2021", aggregate = "region", file = valfile,
             append = TRUE, na_warning = FALSE, try = TRUE, isValidation = TRUE)

}
