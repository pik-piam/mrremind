#' Calculate REMIND variables from historical BP values
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

calcBP <- function() {
  
  mapping <- toolGetMapping("Mapping_BP.csv", type = "reportingVariables") %>%
    mutate(!!sym('conversion') := as.numeric(!!sym('Factor')) * !!sym('Weight')) %>%
    select('variable' = 'BP', 'REMIND', 'conversion', 'unit' = 'Unit_BP', 'Unit_REMIND')
  
  mapping$REMIND <- trimws(mapping$REMIND)
  
  .convert <- function(data){
    data %>% 
      mselect(variable = unique(mapping$variable)) %>%
      as.data.frame() %>% 
      as_tibble() %>% 
      select('region' = 'Region', 'variable' = 'Data1', 
             'year' = 'Year', 'value' = 'Value') %>%
      return()
  }
  
  data <- rbind(
    .convert(readSource("BP", subtype = "Capacity")),
    .convert(readSource("BP", subtype = "Generation"))
  )

  x <- left_join(
    data,
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
              unit = c("EJ"),
              description = "Historical BP values as REMIND variables"))
  
}