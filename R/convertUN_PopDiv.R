
#' Convert UN Population Divison Data
#' 
#' Converts data from \code{readUN_PopDiv()} to ISO country level. "Other, 
#' non-specified areas" is used as a stand-in for Taiwan, Province of China.
#' Countries missing in the data set are set to zero.
#'
#' @param x \code{magclass} object containing UN Population Division data.
#' 
#' @return \code{magclass} object; population in millions.
#' @author Michaja Pehl
#'
#' @importFrom readr read_csv2
#' @importFrom dplyr mutate_ select select_ filter_ arrange_ matches
#' @importFrom lazyeval interp
#' @importFrom quitte add_countrycode_
#' @importFrom countrycode countrycode

convertUN_PopDiv <- function(x) {
  
  target_iso3c <- suppressMessages(
    read_csv2(system.file('extdata', 'iso_country.csv', 
                          package = 'madrat'), 
              col_names = c('country', 'iso3c'), 
              col_types = 'cc', 
              skip = 1)) %>% 
    getElement('iso3c')
  
  x_have <- x %>%
    as.data.frame() %>%
    # convert years to integers
    mutate_(.dots = list(year = interp(~as.integer(as.character(Year))))) %>% 
    select_('year', 'Value', 'Region') %>%
    # add iso3c country codes
    add_countrycode_(c('Region' = 'un'), 'iso3c', warn = FALSE) %>% 
    # use "other, non-specified areas" as proxy for "Taiwan, Province of China"
    mutate_(
      .dots = list(iso3c = interp(~ifelse(Region == '158', 'TWN', iso3c)))) %>% 
    # drop entries from non-countries
    filter_(.dots = interp(~!is.na(iso3c))) %>% 
    # drop m49/Region column
    select(matches('[^(Region)]'))
  
  missing_iso3c <- setdiff(target_iso3c, unique(x_have$iso3c))
  
  # notify about missing countries
  message('Population data for the following countries is not available and',
          'therefore set to 0:\n',
          paste(countrycode(missing_iso3c, 'iso3c', 'country.name'), 
                collapse = ', '))
  
  # fill missing countries with zeros
  x_missing <- expand.grid(year = unique(x_have$year),
                           iso3c = missing_iso3c,
                           Value = 0)
  
  bind_rows(x_have, x_missing) %>%
    arrange_('year', 'iso3c') %>%
    # convert from thousands to millions
    mutate_(.dots = list(Value = interp(~Value / 1000))) %>%
    # reorder columns because as.magpie() does not give a shit about its 
    # parameters and assignes dimensions based on column position
    select_('iso3c', 'year', 'Value') %>%
    as.magpie() %>% 
    return()
}
