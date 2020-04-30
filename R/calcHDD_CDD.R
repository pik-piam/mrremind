#' HDD and CDD scenario and historic data
#' 
#' smoothes the HDD and CDD scenario data
#' 
#' @param tlimit Temperature threshold for computing HDD and CDD: 18, 21 or 25
#' @author Antoine Levesque
#' @importFrom magclass setNames as.magpie getNames lowpass
#' @importFrom dplyr group_by_ arrange_ mutate_ ungroup select_
#' @importFrom quitte as.quitte


calcHDD_CDD <- function(tlimit = 18) {
 
  
  
  data <- readSource("HDD_CDD", subtype = tlimit)
  
  # years.scenario = all years for which history is either NA or 0
  index <- apply(data[,,paste0("history.HDD.history.",tlimit)], c(2,3), function (x) {
    return(all(is.na(x) | x == 0))
  })
  
  years.scenario = c(gsub("y","", attributes(index)$dimnames$period[index]) %>% as.integer(), 2100)
  years.history = setdiff(getYears(data, as.integer = T), years.scenario)
  
  convergence.year = 2030
  convergence.years = c( max(years.history),years.scenario[years.scenario <= convergence.year])
  after.convergence.years = years.scenario[!years.scenario %in% convergence.years]
  lambda = c(rep(1, length(years.history)),
             tail(seq(1,0,length.out = length(convergence.years)),-1 ),
             rep(0, length(after.convergence.years)))
  names(lambda) = c(years.history,years.scenario)
  lambda = as.magpie(lambda)
  
  rcps <- grep("history",getNames(data,T)$rcp, value = TRUE, invert = TRUE)
  ssps <- grep("history",getNames(data,T)$scenario, value = TRUE, invert = TRUE)
  variable <- grep("history",getNames(data,T)$variable, value = TRUE, invert = TRUE)
  
  data[is.na(data)] <- 0
  data <- mbind(data, (1/3)*( setYears(data[,2099,] ,2100) +
                                setYears(data[,2098,] ,2100) +
                                setYears(data[,2097,] ,2100))) #give a point for 2100
  
  # distribute the history between all scenarios
  data <- do.call(mbind,
                 lapply(rcps, function(rcp){
                   do.call(mbind,  lapply(ssps, function(ssp){ 
                     tmp = data[,,rcp][,,ssp]
                      tmp[,years.history,] <- setNames(data[,years.history,"history"][,,"history"],
                                                                  getNames(tmp[,years.history,]))
                      return(tmp)
                    }))
                  } ))
  

  #Smooth the trajectories
  data_smoothed = 
    lapply(ssps, function(scenario){
      lapply( rcps, function(rcp){

        third_dim = paste(scenario,variable,rcp,tlimit,sep = ".")
        out = lowpass(data[,,third_dim],i = 100)
      })
    })
  
  data_smoothed = 
    do.call("mbind",
            unlist(data_smoothed, recursive = F))

  
  # Smooth of the transition between historical and scenario values
  tmp = data * lambda + data_smoothed *(1 -lambda)
  
  
    
  # for negative values that appeared after the filtering replace by 0. Large negative values only occur for very small countries
  tmp[which(tmp < 0)] <- 0
  
  # Set regions where there is either no value for history or for scenarios to 0
  tmp["GRL",,] <- 0
  tmp[c("ASM", "FSM", "GUM","MDV","MHL","TUV","TKL","SJM"),,] <- 0
  
  data <- tmp

  #Add the no Climate Change scenario
  noC = data[,,"rcp2p6"]
  #Fill with the mean of the five last periods
  noC[,years.scenario,] = dimSums(noC[,c((max(years.history)-4):max(years.history)),], dim = 2) / 5
  getNames(noC) = gsub("rcp2p6","rcpNoC", getNames(noC))
  data = mbind(data,noC)
  
  

  
  pop <- calcOutput("Population",aggregate=F, FiveYearSteps = F)
  getNames(pop) <- tolower(sub("pop_(....).?$","\\1",getNames(pop)))
  pop = pop[,,ssps]
  #y <- c(seq(1971,2010,by = 1), seq(2015,2100 by = 5))
  y <- intersect(getYears(data), getYears(pop))
  data <- data[,y,]
  pop <- pop[,y,]

  

  
  return(list(x= data,weight=pop,
              unit = "HDD or CDD",
              description = "Heating degree days or cooling degree days"))
}
