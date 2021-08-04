#' Convert AGEB data
#' 
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from 
#'          [`readJRC_IDEES()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Falk Benke
#' 
#' @importFrom dplyr %>% mutate select
#' @importFrom madrat getISOlist
#' @importFrom magclass as.data.frame as.magpie
#' @importFrom quitte character.data.frame
#' @importFrom rlang sym syms
#' @importFrom tibble as_tibble
#' @importFrom tidyr complete nesting
#' 
#' @export
convertAGEB <- function(x)
{
  
  x %>%
    as.data.frame() %>% 
    as_tibble() %>% 
    select('region' = 'Region', 'variable' = 'Data1', 'unit' = 'Data2', 
           'year' = 'Year', 'value' = 'Value') %>% 
    character.data.frame() %>% 
    mutate(year = as.integer(!!sym('year'))) %>%
    complete(nesting(!!!syms(c('variable', 'unit', 'year'))),
             region = setNames(getISOlist(), NULL)) %>% 
    as.magpie(tidy = TRUE) %>% 
    return()
}