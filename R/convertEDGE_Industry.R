#' Convert EDGE Industry Data
#' 
#' Convert EDGE Industry Data
#'
#' @param x A [`magpie`][magclass::magclass] object.
#' @param subtype Currently only
#'   * `p29_capitalQuantity_industry` for industry subsector energy efficiency 
#'     capital trajectories
#'
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Michaja Pehl
#' 
#' @seealso [`readSource()`]
#' 
#' @importFrom madrat toolGetMapping getConfig calcOutput
#' @importFrom magclass as.data.frame as.magpie
#' @importFrom dplyr as_tibble select mutate sym inner_join left_join group_by
#'   ungroup
#' @importFrom quitte character.data.frame 
#' @importFrom readr read_delim
#'
#' @export
convertEDGE_Industry <- function(x, subtype) {
  # list all available subtypes with functions doing all the work
  switchboard <- list(
    p29_capitalQuantity_industry = function(x) {
      x %>% 
        as.data.frame() %>% 
        as_tibble() %>% 
        select('region' = 'Region', 'period' = 'Year', 'scenario' = 'Data1',
               'pf' = 'Data2', 'value' = 'Value') %>% 
        character.data.frame() %>% 
        mutate(!!sym('period') := as.integer(!!sym('period'))) %>% 
        inner_join(
          toolGetMapping(name = 'regionmappingH12.csv', type = 'regional') %>% 
            as_tibble() %>% 
            select(iso3c = 'CountryCode', region = 'RegionCode'),
          
          'region'
        ) %>% 
        left_join(
          calcOutput(type = 'GDPppp', aggregate = FALSE, 
                     years = unique(quitte::remind_timesteps$period)) %>% 
            as.data.frame() %>% 
            as_tibble() %>% 
            select('iso3c' = 'Region', 'period' = 'Year', 'scenario' = 'Data1',
                   'GDP' = 'Value') %>% 
            character.data.frame() %>% 
            mutate(!!sym('period') := as.integer(!!sym('period'))),
          
          c('iso3c', 'period', 'scenario')
        ) %>% 
        group_by(!!sym('period'), !!sym('region'), !!sym('scenario'), 
                 !!sym('pf')) %>% 
        mutate(!!sym('value') := !!sym('value') 
                               * !!sym('GDP') / sum(!!sym('GDP'))) %>% 
        ungroup() %>% 
        select('iso3c', 'period', 'scenario', 'pf', 'value') %>% 
        as.magpie(spatial = 1, temporal = 2) %>% 
        return()
    }
  )
    
  # check if the subtype called is available
  if (is_empty(intersect(subtype, names(switchboard)))) {
    # if not, pass input through
    return(x)
  } else {
    # convert some data
    return(switchboard[[subtype]](x))
  }
}
