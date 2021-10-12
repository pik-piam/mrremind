#' Calculate REMIND emission variables from historical UNFCCC values
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate left_join
#' @importFrom madrat toolGetMapping toolCountryFill
#' @importFrom magclass as.magpie mselect
#' @importFrom rlang sym
#' @importFrom stats aggregate
#' @export
calcUNFCCC <- function() {

  data <- readSource("UNFCCC")

  mapping <- toolGetMapping("Mapping_UNFCCC.csv", type = "reportingVariables") %>%
    mutate(!!sym('conversion') := as.numeric(!!sym('Factor')) * !!sym('Weight')) %>%
    select('variable' = 'UNFCCC', 'REMIND', 'conversion', 'unit' = 'Unit_UNFCCC', 'Unit_REMIND')
  
  mapping$variable <- gsub(pattern = "\\.", replacement = "_", mapping$variable) %>% trimws()
  mapping$REMIND <- trimws(mapping$REMIND)
  
  x <- left_join(
    data %>% 
      mselect(variable = unique(mapping$variable)) %>%
      as.data.frame() %>% 
      as_tibble() %>% 
      select('region' = 'Region', 'variable' = 'Data1', 'unit' = 'Data2', 
             'year' = 'Year', 'value' = 'Value'),
    mapping,
    by = 'variable'
  ) %>% 
    filter(!!sym("REMIND") != "") %>%
    mutate(!!sym('value') := ifelse(
      is.na(!!sym('value')), 0,  !!sym('value') * !!sym('conversion')),
      !!sym('REMIND') := paste0(!!sym('REMIND'),  " (", !!sym('Unit_REMIND'), ")")) %>%
    select('variable' = 'REMIND', 'region', 'year', 'value')
  
  x <- aggregate(value ~ variable+region+year, x, sum) %>% 
    as.magpie() %>% 
    toolCountryFill(fill = 0)
  
  return(list(x = x, weight = NULL, 
              unit = c("kt CO2", "kt CH4", "kt N2O"),
              description = "Historical UNFCCC values as REMIND variables"))
}