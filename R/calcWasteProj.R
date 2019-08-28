#' @title calcWasteProj
#' @description Calculates all waste projections, multiplies shares properly by pc or total generation quantities
#' @param pc per capita (kg/capita) or total (Mt)
#' @author David Chen
#' @return magpie object of waste projections by treatment and type 

#'#' @importFrom tidyverse spread select inner_join filter
#'#' @importFrom tidyr unite
#'#' @importFrom DirichletReg DR_data DirichReg predict
calcWasteProj <-function(pc=TRUE){

trt <- calcOutput("WasteDirTrt", aggregate=FALSE)
typ <- calcOutput("WasteDirType", aggregate=FALSE)

#trt shares are per treatment type, following makes them sum up to 1 overall
shares <- trt
shares[,,"organic"] <- trt[,,"organic"]*typ[,,"organic"]
shares[,,"paper"] <- trt[,,"paper"]*typ[,,"paper"]
shares[,,"plastic"] <- trt[,,"plastic"]*typ[,,"plastic"]
shares[,,"glass"] <- trt[,,"glass"]*typ[,,"glass"]
shares[,,"metal"] <- trt[,,"metal"]*typ[,,"metal"]
shares[,,"other"] <- trt[,,"other"]*typ[,,"other"]
shares<-dimOrder(shares, c(2,3,1))
shares <-shares[,,c("SSP1","SSP2","SSP3","SSP4","SSP5")]

gen <-  calcOutput("WasteGen", aggregate=FALSE, pc=FALSE) 

total <- trt[,,c("SSP1","SSP2","SSP3","SSP4","SSP5")]
total[,,"SSP1"] <- dimSums(gen[,,"SSP1"])*shares[,,"SSP1"]
total[,,"SSP2"] <- dimSums(gen[,,"SSP2"])*shares[,,"SSP2"]
total[,,"SSP3"] <- dimSums(gen[,,"SSP3"])*shares[,,"SSP3"]
total[,,"SSP4"] <- dimSums(gen[,,"SSP4"])*shares[,,"SSP4"]
total[,,"SSP5"] <- dimSums(gen[,,"SSP5"])*shares[,,"SSP5"]

total <- dimOrder(total,c(2,3,1))

#calibrate quantities
nl <- calcOutput("NlWasteDistrib", aggregate=F)
nl[,,"other"] <- nl[,,"other"] + nl[,,"wood"] + nl[,,"rubber"]
nl <- nl[,,c("wood","rubber"), invert=T]
nl <- round(nl,4)
nl[nl<0] <- 0
nl <- dimOrder(nl, c(2,1))

 
gen <- time_interpolate(gen, interpolated_year= getYears(nl) , extrapolation_type = "linear")
regions <- intersect(getRegions(gen), getRegions(nl))
years <- intersect(getYears(gen), getYears(nl))
real <- nl[regions,years,]*gen[regions,years,]

reg_values <- time_interpolate(total, interpolated_year= getYears(nl) , extrapolation_type = "linear")

c_factor <- dimSums(real[,,"SSP2"]/reg_values[regions,,"SSP2"], dim=2, na.rm=T)

c_factor[which(is.infinite(c_factor))] <- 1

total[regions,,] <- total[regions,,]*collapseNames(setYears(c_factor,NULL))

if (pc==TRUE){
  pop <- calcOutput("Population", aggregate=FALSE)
  getNames(pop) <- gsub("pop_","",getNames(pop))
  pop<- pop[,,c("a1","a2","b1","b2"), invert=TRUE]

  
  
    total <- (total*10^9)/(pop*10^6)   #convert to kilos
 
   weight=pop[,,c("SSP1","SSP2","SSP3","SSP4","SSP5")]
  unit="kg/cap"
}
else if (pc==FALSE){
  weight=NULL
  unit="Mt"
}

#calibrate

return(list(
  x=total,
  weight=weight,
  unit=unit,
  description="total waste generation"))
}

