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
#' @importFrom dplyr %>% distinct filter full_join group_by left_join mutate 
#'                  pull select
#' @importFrom madrat getISOlist
#' @importFrom magclass as.data.frame as.magpie
#' @importFrom quitte add_countrycode_ character.data.frame
#' @importFrom rlang sym syms
#' @importFrom tibble as_tibble
#' @importFrom tidyr complete nesting
#' 
#' @export
convertJRC_IDEES <- function(x)
{
  x %>% 
    # clean madbrat data
    as.data.frame() %>% 
    as_tibble() %>% 
    select('region' = 'Data1', 'variable' = 'Data2', 'unit' = 'Data3', 
           'year' = 'Year', 'value' = 'Value') %>% 
    character.data.frame() %>% 
    mutate(year = as.integer(!!sym('year'))) %>% 
    # convert Eurostat to ISO3C contry codes
    add_countrycode_(origin = c('region' = 'eurostat'), 
                     destination = 'iso3c', 
                     warn = FALSE, na.rm = TRUE) %>% 
    select(-'region') %>% 
    # extend to all countries
    complete(nesting(!!!syms(c('variable', 'unit', 'year'))),
             iso3c = setNames(getISOlist(), NULL)) %>% 
    as.magpie(tidy = TRUE) %>% 
    return()
}
