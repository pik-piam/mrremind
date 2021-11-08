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
    
    factors <- toolGetMapping("BP_Renewable_Efficiency_Factors.csv", type = "sectoral")
    colnames(factors) <- c("year", "factor")
    factors <- rbind(
      factors,
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

  # calculate net oil trade
  trade.oil <- readSource("BP", subtype = "Trade Oil")
  trade.oil.net <- trade.oil[,,"Trade|Export|Oil (kb/d)"] - trade.oil[,,"Trade|Import|Oil (kb/d)"]
  getNames(trade.oil.net) <- c("Net Trade|Oil (kb/d)")
  getSets(trade.oil.net) <- c("region", "year", "data")
  
  # calculate net gas trade
  trade.gas <- readSource("BP", subtype = "Trade Gas")
  trade.gas.net <- trade.gas[,,"Trade|Export|Gas (bcm)"] - trade.gas[,,"Trade|Import|Gas (bcm)"]
  getNames(trade.gas.net) <- c("Net Trade|Gas (bcm)")
  getSets(trade.gas.net) <- c("region", "year", "data")
  
  # calculate net coal trade
  trade.coal <- readSource("BP", subtype = "Trade Coal")
  trade.coal.net <- trade.coal[,,"Trade|Export|Coal (EJ)"] - trade.coal[,,"Trade|Import|Coal (EJ)"]
  getNames(trade.coal.net) <- c("Net Trade|Coal (EJ)")
  getSets(trade.coal.net) <- c("region", "year", "data")
  

  
  # prepare price data
  # ...
  
  data <- rbind(
    .convert(readSource("BP", subtype = "Capacity")),
    .convert(readSource("BP", subtype = "Generation")),
    .convert(consumption.fossils),
    .convert(consumption.renewables),
    .convert(consumption.pe),
    .convert(trade.oil),
    .convert(trade.oil.net),
    .convert(trade.gas),
    .convert(trade.gas.net),
    .convert(trade.coal),
    .convert(trade.coal.net)
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
              unit = c("EJ", "EJ/yr", "GW"), # TODO
              description = "Historical BP values as REMIND variables"))
  
}