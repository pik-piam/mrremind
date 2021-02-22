#' Convert JRC IDEES data
#' 
#' @md
#' Missing data for EU-28 countries is added, by distributing the difference of
#' `EU28` and the sum of country-values based on countries share in EU-28 GDP.
#'
#' @param x A [`magpie`][magclass::magclass] object returned from 
#'          [`readJRC_IDEES()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Michaja Pehl
#' 
#' @seealso [`readJRC_IDEES()`]
#' 
#' @importFrom dplyr distinct filter full_join group_by left_join mutate pull 
#'                   select
#' @importFrom madrat calcOutput getISOlist toolGetMapping
#' @importFrom magclass as.data.frame as.magpie
#' @importFrom quitte add_countrycode_ character.data.frame
#' @importFrom rlang sym syms
#' @importFrom tibble as_tibble
#' @importFrom tidyr complete nesting
#' 
#' @export
convertJRC_IDEES <- function(x)
{
  # clean up madbrat data
  x <- x %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select('region' = 'Data1', 'variable' = 'Data2', 'unit' = 'Data3', 
           'year' = 'Year', 'value' = 'Value') %>% 
    character.data.frame() %>% 
    mutate(!!sym('year') := as.integer(!!sym('year')),
           !!sym('variable') := sub('_', '.', !!sym('variable')),
           !!sym('unit') := sub('_', '.', !!sym('unit')))
  
  
  # intensive values are not extended to missing countries
  intensive_variables <- x %>% 
    distinct(!!sym('variable'), !!sym('unit')) %>% 
    filter(grepl('/', !!sym('unit'))) %>% 
    pull(!!sym('variable'))
  
  EUR_iso3c <- toolGetMapping(name = 'regionmapping_21_EU11.csv',
                 type = 'regional') %>% 
    as_tibble() %>% 
    filter('EUR' == !!sym('missingH12')) %>% 
    pull('CountryCode') %>% 
    sort()
  
  EUR_GDP <- calcOutput(type = 'GDPpppPast', years = unique(x$year), 
                        aggregate = FALSE) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select('iso3c' = 'Region', 'year' = 'Year', 'GDP' = 'Value') %>% 
    character.data.frame() %>% 
    filter(!!sym('iso3c') %in% EUR_iso3c) %>% 
    mutate(!!sym('year') := as.integer(!!sym('year')))
  
  x <- full_join(
    left_join(
      x %>%
        filter('EU28' != !!sym('region'),
               !(!!sym('variable') %in% intensive_variables)) %>%
        add_countrycode_(c('region' = 'eurostat'), 'iso3c') %>%
        select(-'region') %>% 
        complete(nesting(!!!syms(c('variable', 'unit', 'year'))),
                 'iso3c' = EUR_iso3c),
      
      EUR_GDP,
      
      c('iso3c', 'year')
    ),

    left_join(
      x %>%
        filter('EU28' == !!sym('region'),
               !(!!sym('variable') %in% intensive_variables)) %>%
        select(-'region', 'value_EU28' = 'value'),
      
      EUR_GDP %>% 
        group_by(!!sym('year')) %>% 
        summarise(!!sym('GDP_EU28') := sum(!!sym('GDP')), .groups = 'drop'),
      
      'year'
    ),
    
    c('variable', 'unit', 'year')
  ) %>% 
    group_by(!!!syms(c('variable', 'unit', 'year'))) %>% 
    mutate(
      !!sym('value') := ifelse(
        !is.na(!!sym('value')), !!sym('value'), 
        ( !!sym('GDP') * is.na(!!sym('value')) 
        / sum(!!sym('GDP') * is.na(!!sym('value'))) 
        * (!!sym('value_EU28') - sum(!!sym('value'), na.rm = TRUE))
        )
      )) %>% 
    ungroup() %>% 
    select(-'value_EU28', -'GDP', -'GDP_EU28')
  
  x %>% 
    complete(nesting(!!!syms(c('variable', 'unit', 'year'))),
             iso3c = setNames(getISOlist(), NULL)) %>% 
    as.magpie(tidy = TRUE) %>% 
    return()
}
