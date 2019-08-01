
#' Read UN Population Division Data
#' 
#' Read past UN population data. Covers 1950 to 2015 yearly and per M.49 area.
#' See \emph{United Nations, Department of Economicand Social Affairs, 
#' Population Division} "World Population Prospects: The 2015 Revision" 
#' (\href{https://esa.un.org/unpd/wpp/}{website}).
#' 
#' @return \code{magclass} object; population in thousands.
#' @author Michaja Pehl
#' @seealso \code{\link{readSource}}
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr select matches
#' @importFrom tidyr gather
#' @importFrom assertr assert

readUN_PopDiv <- function() {
  read_excel('WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS', 
             sheet = 'ESTIMATES',
             skip = 16) %>%
    select('Country code', matches('^[0-9]{4}$')) %>% 
    gather('year', 'value', matches('^[0-9]{4}$'), na.rm = TRUE, 
           convert = TRUE) %>% 
    assert(is.integer, 'year') %>% 
    as.magpie(spatial = 'Country code', temporal = 'year')
}
