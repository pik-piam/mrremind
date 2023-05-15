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
  # get region mappings for aggregation ----
  # Determines all regions data should be aggregated to by examining the columns
  # of the `regionmapping` and `extramappings` currently configured.

  rel <- 'global'   # always compute global aggregate
  for (mapping in c(getConfig("regionmapping"), getConfig("extramappings"))) {
    columns <- setdiff(colnames(toolGetMapping(mapping, 'regional')),
                       c('X', 'CountryCode'))

    if (any(columns %in% rel))
      warning("The following column(s) from ", mapping,
              " exist in another mapping an will be ignored: ",
              paste(columns[columns %in% rel], collapse = ", "))

    rel <- unique(c(rel, columns))
  }

  columnsForAggregation <- gsub("RegionCode", "region",
                                paste(rel, collapse = "+"))

  # default arguments ----
  # default arguments used for calling all subsequent `calc` functions
  default_arguments <- list(
    file = 'historical.mif',
    append = TRUE,
    warnNA = FALSE,
    aggregate = columnsForAggregation,
    try = TRUE)

  # list `calc` functions to call ----
  # list of all `calc` functions to call, as well as their specific arguments
  # arguments can be empty (`list()`), and can overwrite default arguments if
  # needed
  historical_data_calls <- list(
    'Historical'   = list(round = 5),
    'IEA_ETP'      = list(isValidation = TRUE),
    'IEA_WEO_2021' = list(isValidataion = TRUE),
    'UNIDO'        = list(subtype = 'INDSTAT2') # industry value added
    )

  # call all listed functions ----
  for (i in seq_along(historical_data_calls)) {
    function_name <- names(f[i])
    function_arguments <- c(default_arguments[setdiff(names(default_arguments),
                                                      names(f[i][[1]]))],
                            f[i][[1]])

    # do not append for the first call, ensuring an empty file to start with
    if (i == 1)
      function_arguments[['append']] <- FALSE

    do.call('calcOutput', c(list(type = function_name), function_arguments))
  }
}
