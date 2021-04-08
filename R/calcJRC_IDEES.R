#' Calculate selected REMIND energy and emission variables from historical JRC IDEES values 
#' 
#' @md
#'
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Falk Benke
#' 
#' 
#' @importFrom dplyr select mutate left_join
#' @importFrom madrat toolGetMapping toolCountryFill
#' @importFrom magclass as.magpie mbind mselect
#' @importFrom rlang sym
#' @importFrom stats aggregate

calcJRC_IDEES <- function() {
  
  mapping <- toolGetMapping("Mapping_JRC_IDEES_REMIND.csv", type = "reportingVariables") %>%
    mutate(!!sym('conversion') := as.numeric(!!sym('Factor')) * !!sym('Weight')) %>%
    select('variable' = 'JRC_complete', 'REMIND_variable', 'conversion', 'unit' = 'Unit_JRC', 'Unit_REMIND')

  mapping$variable <- gsub(pattern = "\\.", replacement = "_", mapping$variable) %>% trimws()
  mapping$REMIND_variable <- trimws(mapping$REMIND_variable)
  
  ind <- readSource("JRC_IDEES", subtype = "Industry")
  emi <- readSource("JRC_IDEES", subtype = "Emission")
  energy <- readSource("JRC_IDEES", subtype = "Energy")
  
  cntr <- toolGetMapping("regionmappingH12.csv")
  EU28_regions <- cntr[which(cntr$RegionCode == "EUR"),]$CountryCode
  
  x <- left_join(
    mbind(ind, emi, energy) %>% 
      mselect(iso3c = EU28_regions, variable = unique(mapping$variable)) %>%
      as.data.frame() %>% 
      as_tibble() %>% 
      select('region' = 'Region', 'year' = 'Year', 'variable' = 'Data1',
             'unit' = 'Data2', 'value' = 'Value'),
    mapping,
    by = c('variable', 'unit')
  ) %>% 
    mutate(!!sym('value') := ifelse(
      is.na(!!sym('value')), 0,  !!sym('value') * !!sym('conversion')),
      !!sym('REMIND_variable') := paste0(!!sym('REMIND_variable'),  " (", !!sym('Unit_REMIND'), ")")) %>%
    select('variable' = 'REMIND_variable', 'region', 'year', 'value')
  
  x <- aggregate(value ~ variable+region+year, x, sum) %>% 
    as.magpie() %>% 
    toolCountryFill(fill = 0)
  
  return(list(x = x, weight = NULL, 
              unit = "billion US$2005/yr, EJ/yr, Mt CO2/yr, Mt/yr",
              description = "Historical JRC IDEES values as REMIND variables"))
}