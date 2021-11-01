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
#' @importFrom readxl read_excel
#' @importFrom rlang sym
#' @importFrom stats aggregate
#' @export

calcBP <- function() {
  
  .readFactors <- function(){
    filename <- c("bp-stats-review-2021-all-data.xlsx")
    
    factors <- read_excel(filename, sheet = "Methodology", range = "B23:F34") %>% select(-3)
    colnames(factors) <- rep(c("year", "factor"), times = 2)
    factors <- rbind(
      factors[,c(1,2)],
      factors[,c(3,4)],
      data.frame(year = seq(1965,2000,1), factor = 0.36)
    ) %>% 
    filter(!is.na(as.numeric(!!sym("year"))))
    
    return(factors[order(factors$year),])
  }
  
  mapping <- toolGetMapping("Mapping_BP.csv", type = "reportingVariables") %>%
    mutate(!!sym('conversion') := as.numeric(!!sym('Factor')) * !!sym('Weight')) %>%
    select('variable' = 'BP', 'REMIND', 'conversion', 'unit' = 'Unit_BP', 'Unit_REMIND')
  
  mapping$REMIND <- trimws(mapping$REMIND)
  
  .convert <- function(data){
    data %>% 
      mselect(data = unique(mapping$variable)) %>%
      as.data.frame() %>% 
      as_tibble() %>% 
      select('region' = 'Region', 'variable' = 'Data1', 
             'year' = 'Year', 'value' = 'Value') %>%
      return()
  }
  
  # prepare consumption data

  consumption <- readSource("BP", subtype = "Consumption")
  
  # rescale renewables to direct equivalence method by multiplying with the numbers given on sheet "Methodology"
  renewables.factors <- .readFactors() %>% as.magpie() %>% dimReduce()
  renewables.vars <- c("Solar Consumption (EJ)", "Wind Consumption (EJ)", "Nuclear Consumption (EJ)", "Hydro Consumption (EJ)")
  consumption.renewables <- consumption[,,renewables.vars] * renewables.factors 
  
  consumption.fossils <- consumption[,,c(renewables.vars, "Primary Energy Consumption (EJ)"), invert = T]
  
  # recalculate to direct equivalent accounting
  
  # 1. deduct nuclear and electricity generation by renewables
  pe.elec.renewable <-  readSource("BP", subtype = "Generation")[,,"Generation|Electricity|Renewable (EJ)"]
  pe.nuclear <- consumption[,,"Nuclear Consumption (EJ)"]
  consumption.pe <- consumption[,, "Primary Energy Consumption (EJ)"] - pe.nuclear - pe.elec.renewable
    
  # 2. add adjusted nuclear and renewables values
  pe.elec.renewable.dea <- pe.elec.renewable * renewables.factors
  pe.nuclear.dea <- pe.nuclear * renewables.factors
  consumption.pe <- consumption.pe + pe.elec.renewable.dea + pe.nuclear.dea

  
  # prepare price data
  # ...
  
  data <- rbind(
    .convert(readSource("BP", subtype = "Capacity")),
    .convert(readSource("BP", subtype = "Generation")),
    .convert(consumption.fossils),
    .convert(consumption.renewables),
    .convert(consumption.pe),
    .convert(readSource("BP", subtype = "Trade Oil"))
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