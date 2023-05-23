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

  rel <- "global" # always compute global aggregate
  for (mapping in c(getConfig("regionmapping"), getConfig("extramappings"))) {
    columns <- setdiff(
      colnames(toolGetMapping(mapping, "regional")),
      c("X", "CountryCode")
    )

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

  calcOutput(
    type = "IEA_WEO_2021", aggregate = "global", file = valfile,
    append = TRUE, warnNA = FALSE, try = TRUE, isValidation = TRUE
  )

  calcOutput(
    type = "IEA_WEO_2021", aggregate = "region", file = valfile,
    append = TRUE, warnNA = FALSE, try = TRUE, isValidation = TRUE
  )

  ## industry value added ----
  calcOutput(
    type = "UNIDO", subtype = "INDSTAT2", file = valfile,
    aggregate = columnsForAggregation, append = TRUE, warnNA = FALSE,
    try = TRUE
  )

  # filter variables that are too imprecise on regional level

  valpath <- paste0(getConfig("outputfolder"), "/", valfile)

  h <- read.report(file = valpath, as.list = F)

  # remove IEA ETP Steel and Cement Production on sub-global level
  h[, , "IEA ETP", pmatch = T][, , c(
    "Production|Industry|Cement (Mt/yr)",
    "Production|Industry|Steel (Mt/yr)"
  )][setdiff(getItems(h, dim = 1), "GLO"),,] <- NA

  write.report(h, file = valpath)
}
