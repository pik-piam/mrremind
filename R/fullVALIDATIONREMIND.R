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

  rel <- "global"   # always compute global aggregate
  for (mapping in c(getConfig("regionmapping"), getConfig("extramappings"))) {
    columns <- setdiff(colnames(toolGetMapping(mapping, "regional")),
                       c("X", "CountryCode"))

    if (any(columns %in% rel)) {
      warning(
        "The following column(s) from ", mapping,
        " exist in another mapping an will be ignored: ",
        paste(columns[columns %in% rel], collapse = ", ")
      )
    }

    rel <- unique(c(rel, columns))
  }

  columnsForAggregation <- gsub(
    "RegionCode", "region",
    paste(rel, collapse = "+")
  )

  # historical data ----
  valfile <- "historical.mif"

  calcOutput("Historical",
    round = 5, file = valfile, aggregate = columnsForAggregation,
    append = FALSE, warnNA = FALSE, try = TRUE
  )

  calcOutput(
    type = "IEA_ETP", aggregate = columnsForAggregation, file = valfile,
    append = TRUE, warnNA = FALSE, try = TRUE, isValidation = TRUE
  )

  ## industry value added ----
  calcOutput(
    type = "UNIDO", subtype = "INDSTAT2", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = TRUE
  )

  ## add WEO data on regional and global level ----
  weo <- calcOutput(
    type = "IEA_WEO_2021", subtype = "global", aggregate = columnsForAggregation,
    warnNA = FALSE, try = TRUE, isValidation = TRUE
  )

  weo <- weo["GLO", , ]
  write.report(weo, file = valfile, append = TRUE)

  weo <- calcOutput(
    type = "IEA_WEO_2021", subtype = "region", aggregate = columnsForAggregation,
    warnNA = FALSE, try = TRUE, isValidation = TRUE
  )

  weo <- weo["GLO", , invert = TRUE]
  write.report(weo, file = valfile, append = TRUE)

  # filter variables that are too imprecise on regional level ----
  filter_historical_mif()

}
