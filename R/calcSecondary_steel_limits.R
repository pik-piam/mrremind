#' Calculate Secondary Steel Limits
#'
#' Upper limit to secondary steel production due to scrap availability.
#' 
#' @return A [`MAgPIE`][magclass::magclass] object.
#'
#' @author Michaja Pehl
#' 
#' @importFrom dplyr as_tibble filter matches mutate select sym
#' @importFrom madrat readSource
#' @importFrom magclass as.data.frame as.magpie getYears 
#' @importFrom quitte interpolate_missing_periods
#' @importFrom tidyr pivot_longer pivot_wider

#' @export
calcSecondary_steel_limits <- function() {
  .years <- getYears(readSource('EDGE', subtype = 'FE_stationary'))
  x <- readSource(type = 'EDGE_Industry', 
                            subtype = 'secondary_steel_limits') %>%
    as.data.frame() %>% 
    as_tibble() %>% 
    select(-'Data2') %>% 
    pivot_wider(names_from = 'Data1', values_from = 'Value') %>% 
    mutate(!!sym('SDP') := !!sym('SSP1')) %>% 
    pivot_longer(matches('^S[SD]P[1-5]?$'), names_to = 'scenario') %>% 
    mutate(!!sym('Year') := as.integer(as.character(!!sym('Year')))) %>% 
    interpolate_missing_periods(
      Year = as.integer(sub('^y', '', .years)),
      expand.values = TRUE) %>% 
    mutate(!!sym('year') := paste0('y', !!sym('Year')),
           !!sym('scenario') := paste0('gdp_', !!sym('scenario')),
           # t * 1e-9 Gt/t = t
           !!sym('value') := !!sym('value') * 1e-9) %>% 
    select('Region', 'year', 'scenario', 'value') %>% 
    filter(!!sym('year') %in% .years) %>% 
    as.magpie()
  
  return(
    list(x = x,
         weight = NULL,
         unit = 'Gt/year',
         description = paste('Upper limit to secondary steel production due',
                             'to scrap availability'),
         structure.data = '^gdp_(SSP[1-5]|SDP)')
  )
}
