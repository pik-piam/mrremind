#' Filter improperly aggregated regional data from historical.mif
#'
#' Load a `historical.mif` file, remove designated data, write back to file.
#'
#' For all combinations of scenario, model, and variable (and all years), either
#' all regions listed in `exclude_regions` in the `filter_table` are removed, or
#' all regions _except_ those listed in `include_regions` in the `filter_table`
#' are removed.
#' Defining both `include_regions` and `exclude_regions` on the same row of
#' `filter_table` will throw an error.
#' Scenarios, models, and variables are matched precisely, not partially or via
#' regular expressions.
#'
#' @md
#' @param path Path to `historical.mif` (or any mif for that matter), defaults
#'   to `historical.mif` in the currently configured
#'   `getConfig('outputfolder')`.
#' @param filter_table Data frame with columns `scenario`, `model`, `variable`,
#'   `include_regions`, and `exclude_regions`.  Defaults to the built-in
#'   `./inst/extdata/historical_mif_filter_table.csv` of the `mrremind` package.
#'
#' @importFrom madrat getConfig
#' @importFrom magclass getItems mbind read.report write.report
#' @importFrom quitte cartesian

#' @export
filter_historical_mif <- function(path = NULL, filter_table = NULL) {
  if (is.null(path))
    path <- file.path(getConfig('outputfolder'), 'historical.mif')

  if (is.null(filter_table))
    filter_table <- read.csv(
      file = system.file('extdata', 'historical_mif_filter_table.csv',
                         package = 'mrremind', mustWork = TRUE),
      colClasses = 'character')

  h <- read.report(file = path, as.list = FALSE)

  for (i in seq_len(nrow(filter_table))) {
    aggregation_filter <- filter_table[i,]

    scenario_model_variable <- Reduce(
      f = cartesian,
      x = aggregation_filter[c('scenario', 'model', 'variable')])

    if (   '' != aggregation_filter[['include_regions']]
           && '' != aggregation_filter[['exclude_regions']]) {
      stop('Both include and exclude regions defined for:',
           paste(c('', scenario_model_variable), collapse = '\n'))
    }

    if ('' != aggregation_filter[['include_regions']]) {
      regions <- aggregation_filter[['include_regions']]
    }
    else {
      regions <- setdiff(getItems(h, dim = 'region'),
                         aggregation_filter[['exclude_regions']])
    }

    h_include <- h[regions,,scenario_model_variable]

    h <- mbind(h[,,scenario_model_variable, invert = TRUE], h_include)
  }

  write.report(h, file = path)
}
