#' industry/subsector change factors
#' 
#' Change factors of specific FE and material demand for the
#' `industry/subsector` realisation of REMIND.
#' 
#' @md
#' @param subtype One of
#'   - `FE` for specific final energy demand change factors
#'   - `material` for specific material demand change factors
#' @param scenarios A vector of scenarios for which factors are to be returned.
#' @param regions A vector of regions for which factors are to be returned.
#'
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' Factors are read from the files `specific_FE.csv` and
#' `specific_material.csv`, respectively.  `NA` is used to mark defaults for the
#' `scenario` and `region` columns, and specified values will overwrite these
#' defaults.
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
               progress = FALSE) %>% 
        madrat_mule()
    },
    
    'material' = function() {
      read_csv(file = 'specific_material.csv',
               col_types = 'cccd',
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
  
  . <- NULL
  
  # subtype switchboard ----
  switchboard <- list(
    'FE' = function() {
      alpha <- readSource(type = 'industry_subsectors_specific', subtype = 'FE',
                          convert = FALSE) %>% 
        madrat_mule()
      
      alpha.global <- alpha %>% 
        filter(is.na(.data$scenario), is.na(.data$region)) %>% 
        complete(nesting(!!!syms(c('subsector', 'alpha'))),
                 scenario = scenarios,
                 region = regions) %>% 
        filter(!is.na(.data$scenario), !is.na(.data$region))
      
      alpha.scenario <- alpha %>% 
        filter(!is.na(.data$scenario), 
               .data$scenario %in% scenarios,
               is.na(.data$region)) %>% 
        complete(nesting(!!!syms(c('scenario', 'subsector', 'alpha'))),
                 region = regions) %>% 
        filter(!is.na(.data$region))
      
      alpha.region <- alpha %>% 
        filter(!is.na(.data$region),
               .data$region %in% regions, 
               is.na(.data$scenario)) %>% 
        complete(nesting(!!!syms(c('region', 'subsector', 'alpha'))),
                 scenario = scenarios) %>% 
        filter(!is.na(.data$scenario))
      
      alpha.scenario.region <- alpha %>% 
        filter(!is.na(.data$scenario), .data$scenario %in% scenarios,
               !is.na(.data$region), .data$region %in% regions)
      
      alpha <- alpha.global %>% 
        anti_join(
          alpha.scenario, 
          
          c('scenario', 'region', 'subsector')
        ) %>% 
        bind_rows(alpha.scenario) %>% 
        anti_join(
          alpha.region, 
          
          c('scenario', 'region', 'subsector')
        ) %>% 
        bind_rows(alpha.region) %>% 
        anti_join(
          alpha.scenario.region, 
          
          c('scenario', 'region', 'subsector')
        ) %>% 
        bind_rows(alpha.scenario.region) %>% 
        select('scenario', 'region', 'subsector', 'alpha') %>% 
        as.magpie(spatial = 0, temporal = 0, data = ncol(.))
      
      return(list(x = alpha, weight = NULL, unit = '', description = ''))
    })
  
  # check if the subtype called is available ----
  if (!subtype %in% names(switchboard)) {
    stop(paste('Invalid subtype -- supported subtypes are:', 
               paste(names(switchboard), collapse = ', ')))
  }
  
  # load data and to whatever ----
  return(switchboard[[subtype]]())
}
