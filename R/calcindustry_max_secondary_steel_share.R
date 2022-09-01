#' Calculate Maximum Secondary Steel Production Share
#'
#' Reads ExpertGuess/industry_max_secondary_steel_share and expands to all
#' `scenarios`/`regions` using default data.  See [`tool_expand_tibble()`] for
#' details.
#'
#' @param scenarios A character vector of scenarios to expand data to.
#' @param regions A character vector of regions to expand data to.
#'
#' @return A list with a [`magpie`][magclass::magclass] object `x`.

#' @export
calcindustry_max_secondary_steel_share <- function(scenarios = NULL,
                                                   regions = NULL) {
  if (is.null(scenarios)) {
    stop('Scenario definitions missing.')
  }

  if (is.null(regions)) {
    stop('Region definitions missing.')
  }

  . <- NULL

  return(list(
    x = readSource(type = 'ExpertGuess',
                   subtype = 'industry_max_secondary_steel_share',
                   convert = FALSE) %>%
      madrat_mule() %>%
      tool_expand_tibble(scenarios, regions) %>%
      pivot_longer(
        !all_of(names(which('character' == unlist(lapply(., typeof)))))) %>%
      as.magpie(spatial = 0, temporal = 0, data = ncol(.)),
    weight = NULL, unit = '', description = ''))
}
