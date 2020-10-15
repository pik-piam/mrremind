#' @title calc Capacity Factor
#' @description provides capacity factor values
#' @return magpie object of the capacity factor data
#' @author Renato Rodrigues, Stephen Bi
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("CapacityFactor")
#' }
#' 

calcCapacityFactor <- function(){
  
  # Read capacity factor inputs
  global <- readSource("REMIND_11Regi", subtype="capacityFactorGlobal", convert = FALSE)
  # Set coal plant capacity factor long-term assumption to 50% (down from 60%)
  global[,,"pc"] <- 0.5
  # Read capacity factor rules
  rules <- readSource("REMIND_11Regi", subtype="capacityFactorRules")
  
  #   Creating new MAgPIE object to store the final capacity values
  output <- new.magpie(getRegions(rules),seq(2005,2150,5),getNames(global)) 
  
  
  # Merging global and rules values
  # Filling MagPIE object with global values
  output[,,getNames(global)] <- global[,,getNames(global)]
  # Overwritting MAgPie object with rules values
  output[getRegions(rules),getYears(rules),getNames(rules)] <- ifelse(rules[getRegions(rules),getYears(rules),getNames(rules)]!=0, rules[getRegions(rules),getYears(rules),getNames(rules)], output[getRegions(rules),getYears(rules),getNames(rules)]) 
  
  #Derive coal capacity factor rule assumptions for 2020 - 2035 (linear convergence of historical trends to global default)
  hist_coal <- calcOutput("CapacityFactorHist",aggregate = F)
  hist_coal <- hist_coal[,,"pc"]
  hist_yr <- "y2015"
  start_yr <- "y2020"
  end_yr <- "y2030"
  conv_yr <- "y2035"
  output[,getYears(output)[which(getYears(output)<start_yr)],"pc"] <- hist_coal
  output[,getYears(output)[which(getYears(output)>=conv_yr)],"pc"] <- global[,,"pc"]
  slope <- (output[,conv_yr,'pc'] - output[,hist_yr,'pc']) / (as.numeric(gsub("y","",conv_yr)) - as.numeric(gsub("y","",hist_yr)))
  for (t in getYears(output)[which(getYears(output)>=start_yr & getYears(output)<=end_yr)]) {
    output[,t,"pc"] <- output[,hist_yr,"pc"] + slope * (as.numeric(gsub("y","",t)) - as.numeric(gsub("y","",hist_yr)))
  }
  
  # Convergence and lin.convergence functions are not working...
  # output[,getYears(output)[which(getYears(output)>=hist_yr & getYears(output)<=conv_yr)],"pc"] <- 
  #   convergence(origin=output[,getYears(output)[which(getYears(output)>=hist_yr & getYears(output)<=conv_yr)],"pc"],
  #               aim=as.numeric(global[,,"pc"]),start_year=start_yr,end_year=conv_yr)

  # Define weight aggregation for capacity factors
  # using final energy as a proxy for the existent capacity factor to weight the capacity factor aggregation (it should be changed if the information about the existent capacity factor become available in the future)
  #fe <- calcOutput("FE",source="IEA",aggregate=FALSE)[,2015,"FE (EJ/yr)"]
  
  #Now using IEA total electricity generation as the aggregation weight, may change to generation by tech 
  elec_gen <- calcOutput("IO",subtype="output",aggregate=FALSE,)
  elec_gen <- mselect(elec_gen,TIME=paste0("y",max(as.numeric(gsub("y","",getYears(elec_gen))))-1),data1="seel")
  elec_gen[which(elec_gen<0)] <- 0
  #weight <- new.magpie(getRegions(output),getYears(output),getNames(output))
  # for (te in getNames(output)) {
  #   if (any(grepl(paste0(".",te),getNames(elec_gen),fixed = TRUE))) {
  #     weight[,getYears(weight)<=getYears(elec_gen),te] <- elec_gen[,,te]
  #   }else {
  #     weight[,getYears(weight)<=getYears(elec_gen),te] <- dimSums(elec_gen,dim=3)
  #   }
  #   #weight[getRegions(weight[,,te][which(weight[,,te]==0)]),,te] <- dimSums(elec_gen[getRegions(weight[,,te][which(weight[,,te]==0)]) ,,],dim=3)
  # }
  # weight[,getYears(weight)>getYears(elec_gen),] <- dimSums(elec_gen,dim=3)
  
  weight <- dimSums(elec_gen,dim=3)
  
  # Return regions aggregation weighted by final energy 
  return(list(x=output, weight=weight,
               unit="% of capacity", 
               description="Installed capacity availability - capacity factor (fraction of the year that a plant is running)"              
  ))
  
}

