#' industry/subsector change factors
#'
#' Change factors of specific FE and material demand for the
#' `industry/subsector` realisation of REMIND.
#'
#' @md
#' @param subtype One of
#'   - `FE` for specific final energy demand change factors
#'   - `material_alpha` for alpha factors and convergence time of specific
#'     material demand decreases relative to the `SSP2EU` scenario
#'   - `material_relative` for scaling factors of specific material demand
#'     relative to baseline scenarios
#'   - `material_relative_change` for scaling factors of specific material
#'     demand _change_ relative to baseline scenarios
#'
#' @param scenarios A vector of scenarios for which factors are to be returned.
#' @param regions A vector of regions for which factors are to be returned.
#' @param direct A data frame as returned by
#'   `readindustry_subsectors_specific()` to load debugging/developing data
#'   directly instead of from file.
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @details
#' Factors are read from the files `specific_FE.csv`,
#' `specific_material_alpha.csv`, `specific_material_relative.csv`, and
#' `specific_material_relative_change.csv`, respectively.  `NA` is used to mark
#' defaults for the `scenario` and `region` columns, and specified values will
#' overwrite these defaults.
#'
#' So
#'   - `NA,NA,cement,1` will be extended to all `scenarios` and `regions`
#'   - `scen1,NA,cement,2` will overwrite this default for all `regions` in
#'     `scen1`
#'   - `NA,regi1,cement,3` will overwrite this again for all `scenarios`
#'     (including `scen1`) for `regi1`
#'   - `scen1,regi1,cement,4` will lastly overwrite the value for the `scen1`,
#'     `regi1` combination
#'
#' Replacements occure in this fixed order (`NA`/`NA`, `scenario`/`NA`,
#' `NA`/`region`, `scenario`/`region`).
#'
#' Lastly, output is filtered for `scenarios` and `regions`.
#'
#' For debugging and development, instead of modifying the .csv files in
#' `sources/industry_subsectors_specific/` and interfering with production runs,
#' modify the calling code (e.g. `calcFEdemand.R`) to use `direct` data (entered
#' verbatim or loaded from somewhere else.)
#'
#' @author Michaja Pehl
#'
#' @importFrom dplyr anti_join bind_rows filter select
#' @importFrom quitte madrat_mule
#' @importFrom readr read_csv
#' @importFrom tidyr complete nesting

#' @export
#' @rdname industry_subsector_specific
readindustry_subsectors_specific <- function(subtype = NULL) {
  # file path (for easier debugging)
  path <- '~/PIK/swap/inputdata/sources/industry_subsectors_specific/'
  path <- './'

  # subtype switchboard ----
  switchboard <- list(
    'FE' = function() {
      read_csv(file = file.path(path, 'specific_FE.SSP3.csv'),
               col_types = 'cccd',
               comment = '#',
               progress = FALSE) %>%
        madrat_mule()
    },

    'material_alpha' = function() {
      read_csv(file = file.path(path, 'specific_material_alpha.csv'),
               col_types = 'cccdi',
               comment = '#',
               progress = FALSE) %>%
        madrat_mule()
    },

    'material_relative' = function() {
      read_csv(file = file.path(path, 'specific_material_relative.csv'),
               col_types = 'ccccd',
               comment = '#',
               progress = FALSE) %>%
        madrat_mule()
    },

    'material_relative_change' = function() {
      read_csv(file = file.path(path, 'specific_material_relative_change.csv'),
               col_types = 'ccccd',
               comment = '#',
               progress = FALSE) %>%
        madrat_mule()
    },

    'industry_specific_FE_limits' = function() {
      read_csv(file = file.path(path, 'industry_specific_FE_limits.csv'),
               comment = '#',
               show_col_types = FALSE) %>%
        madrat_mule()
    }
  )

  # check if the subtype called is available ----
  if (!subtype %in% names(switchboard)) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               paste(names(switchboard), collapse = ', ')))
  }

  # load data and to whatever ----
  return(switchboard[[subtype]]())
}

#' @export
#' @rdname industry_subsector_specific
calcindustry_subsectors_specific <- function(subtype = NULL, scenarios = NULL,
                                             regions = NULL, direct = NULL) {
  if (is.null(scenarios)) {
    stop('Scenario definitions missing.')
  }

  if (is.null(regions)) {
    stop('Region definitions missing.')
  }

  . <- NULL

  if (is.null(direct)) {
    x <- readSource(type = 'industry_subsectors_specific', subtype = subtype,
		    convert = FALSE) %>%
      madrat_mule()
  }
  else {
    if (!is.data.frame(direct)) {
      stop('`direct` is not a data frame')
    }
    if (!all(c('scenario', 'region', 'subsector') %in% colnames(direct))) {
      stop('`direct` is missing columns: ',
	   paste(setdiff(c('scenario', 'region', 'subsector'),
			 colnames(direct)),
		 collapse = ', '))
    }
    x <- direct
  }

  return(list(
    x = x %>%
      tool_expand_tibble(scenarios, regions,
			 structure.columns = 'subsector') %>%
      pivot_longer(
        !all_of(names(which('character' == unlist(lapply(., typeof)))))
      ) %>%
      as.magpie(spatial = 0, temporal = 0, data = ncol(.)),
    weight = NULL, unit = '', description = ''))
}
