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
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
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
#' @author Michaja Pehl
#'
#' @importFrom dplyr anti_join bind_rows filter select
#' @importFrom magclass as.magpie
#' @importFrom quitte madrat_mule
#' @importFrom readr read_csv
#' @importFrom tidyr complete nesting

#' @export
#' @rdname industry_subsector_specific
readindustry_subsectors_specific <- function(subtype = NULL) {
  # subtype switchboard ----
  switchboard <- list(
    'FE' = function() {
      read_csv(file = 'specific_FE.csv',
               col_types = 'cccd',
               comment = '#',
               progress = FALSE) %>%
        madrat_mule()
    },

    'material_alpha' = function() {
      read_csv(file = 'specific_material_alpha.csv',
               col_types = 'cccdi',
               comment = '#',
               progress = FALSE) %>%
        madrat_mule()
    },

    'material_relative' = function() {
      read_csv(file = 'specific_material_relative.csv',
               col_types = 'ccccd',
               comment = '#',
               progress = FALSE) %>%
        madrat_mule()
    },

    'material_relative_change' = function() {
      read_csv(file = 'specific_material_relative_change.csv',
               col_types = 'ccccd',
               comment = '#',
               progress = FALSE) %>%
        madrat_mule()
    })

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
                                             regions = NULL) {
  if (is.null(scenarios)) {
    stop('Scenario definitions missing.')
  }

  if (is.null(regions)) {
    stop('Region definitions missing.')
  }

  expand_tibble <- function(d, scenarios, regions) {

    . <- NULL

    # entries with both scenarios and regions defined
    d.scenario.region <- d %>%
      filter(!is.na(.data$scenario), .data$scenario %in% scenarios,
             !is.na(.data$region), .data$region %in% regions)

    # entries with only scenarios defined
    d.scenario <- d %>%
      filter(!is.na(.data$scenario), .data$scenario %in% scenarios,
             is.na(.data$region)) %>%
      complete(nesting(!!!syms(setdiff(colnames(.), 'region'))),
               region = regions) %>%
      filter(!is.na(.data$region))

    # entries with only regions defined
    d.region <- d %>%
      filter(is.na(.data$scenario),
             !is.na(.data$region), .data$region %in% regions) %>%
      complete(nesting(!!!syms(setdiff(colnames(.), 'scenario'))),
               scenario = scenarios) %>%
      filter(!is.na(.data$scenario))

    # entries with neither scenario nor regions defined
    d.global <- d %>%
      filter(is.na(.data$scenario), is.na(.data$region)) %>%
      complete(nesting(!!!syms(setdiff(colnames(.), c('scenario', 'region')))),
               scenario = scenarios,
               region = regions) %>%
      filter(!is.na(.data$scenario), !is.na(.data$region))

    # combine all entries
    d.global %>%
      # scenarios overwrite global data
      anti_join(
        d.scenario,

        c('scenario', 'region', 'subsector')
      ) %>%
      bind_rows(d.scenario) %>%
      # regions overwrite global and scenario data
      anti_join(
        d.region,

        c('scenario', 'region', 'subsector')
      ) %>%
      bind_rows(d.region) %>%
      # specific data overwrites everything
      anti_join(
        d.scenario.region,

        c('scenario', 'region', 'subsector')
      ) %>%
      bind_rows(d.scenario.region) %>%
      select(all_of(colnames(d))) %>%
      return()
  }

  . <- NULL

  return(list(
    x = readSource(type = 'industry_subsectors_specific', subtype = subtype,
                   convert = FALSE) %>%
      madrat_mule() %>%
      expand_tibble(scenarios, regions) %>%
      pivot_longer(
        !all_of(names(which('character' == unlist(lapply(., typeof)))))) %>%
      as.magpie(spatial = 0, temporal = 0, data = ncol(.)),
    weight = NULL, unit = '', description = ''))
}
