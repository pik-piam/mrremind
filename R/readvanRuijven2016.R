#' Read van Ruijven et al. (2016) data.
#' 
#' Read data from van Ruijven et al. 2016,
#' (http://dx.doi.org/10.1016/j.resconrec.2016.04.016,
#' https://www.zotero.org/groups/52011/rd3/items/itemKey/6QMNBEHQ), obtained 
#' through personal communication (e-mail to Michaja Pehl).
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Michaja Pehl
#' 
#' @importFrom dplyr filter mutate select
#' @importFrom magrittr %>%
#' @importFrom quitte add_countrycode_ madrat_mule
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' 
#' @seealso [`readSource()`]

#' @export
readvanRuijven2016 <- function() {
  read_excel(
    path = './Cement_data_from_Bas.xlsx',
    sheet = 'Production',
    range = 'A4:AS321') %>% 
    filter(999 != .data$`Region #`) %>% 
    pivot_longer(cols = matches('^[0-9]{4}$'), names_to = 'year', 
                 names_transform = list(year = as.integer), 
                 values_drop_na = TRUE) %>% 
    add_countrycode_(origin = c('FAO #' = 'fao'), destination = 'iso3c', 
                     warn = FALSE) %>% 
    add_countrycode_(origin = c('FAO Name' = 'country.name'), 
                     destination = c('iso3c.alt' = 'iso3c'), warn = FALSE) %>% 
    mutate(iso3c = ifelse(!is.na(.data$iso3c), .data$iso3c, .data$iso3c.alt),
           # kg * 0.001 t/kg = t
           value = .data$value * 1e-3) %>% 
    filter(!is.na(.data$iso3c)) %>% 
    select('iso3c', 'year', 'value') %>% 
    madrat_mule()
}
