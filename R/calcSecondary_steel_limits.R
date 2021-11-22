#' Calculate Secondary Steel Limits
#'
#' Upper limit to secondary steel production due to scrap availability.
#' 
#' @return A [`MAgPIE`][magclass::magclass] object.
#'
#' @author Michaja Pehl
#' 
#' @importFrom assertr verify
#' @importFrom dplyr as_tibble filter matches mutate n select
#' @importFrom madrat readSource
#' @importFrom magclass as.data.frame as.magpie getYears 
#' @importFrom quitte interpolate_missing_periods
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer pivot_wider

#' @export
calcSecondary_steel_limits <- function() {
  .years <- getYears(readSource('EDGE', subtype = 'FE_stationary'))
  x <- readSource(type = 'EDGE_Industry', 
                  subtype = 'secondary_steel_limits') %>%
    as.data.frame() %>% 
    as_tibble() %>% 
    select(-'Data2') %>% 
    mutate(Year = as.integer(as.character(.data$Year))) %>% 
    interpolate_missing_periods(
      Year = as.integer(sub('^y', '', .years)),
      value = 'Value',
      expand.values = TRUE) %>% 
    mutate(year = paste0('y', .data$Year),
           scenario = paste0('gdp_', .data$Data1),
           # t * 1e-9 Gt/t = t
           value = .data$Value * 1e-9) %>% 
    select('Region', 'year', 'scenario', 'value') %>% 
    filter(.data$year %in% .years) %>% 
    group_by(.data$Region, .data$year, .data$scenario) %>% 
    mutate(count = n()) %>% 
    verify(1 == .data$count) %>% 
    ungroup() %>% 
    select(-'count') %>% 
    as.magpie()
  
  return(
    list(x = x,
         weight = NULL,
         unit = 'Gt/year',
         description = paste('Upper limit to secondary steel production due',
                             'to scrap availability'),
         structure.data = '^gdp_(SSP|SDP)')
  )
}
