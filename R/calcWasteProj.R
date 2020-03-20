#' @title calcWasteProj
#' @description Calculates all waste projections, multiplies shares properly by pc or total generation quantities
#' @param pc per capita (kg/capita) or total (Mt)
#' @param SSP SSP scenario
#' @author David Chen
#' @return magpie object of waste projections by treatment and type 

#'#' @importFrom tidyverse spread select inner_join filter
#'#' @importFrom tidyr unite
#'#' @importFrom DirichletReg DR_data DirichReg predict
calcWasteProj <-function(pc=TRUE, SSP="SSP2"){

trt <- calcOutput("WasteTrt", aggregate=FALSE)
typ <- calcOutput("WasteType", aggregate=FALSE)

#trt shares are per treatment type, following makes them sum up to 1 overall
shares <- trt
shares[,,"organic"] <- trt[,,"organic"]*typ[,,"organic"]
shares[,,"paper"] <- trt[,,"paper"]*typ[,,"paper"]
shares[,,"plastic"] <- trt[,,"plastic"]*typ[,,"plastic"]
shares[,,"glass"] <- trt[,,"glass"]*typ[,,"glass"]
shares[,,"metal"] <- trt[,,"metal"]*typ[,,"metal"]
shares[,,"other"] <- trt[,,"other"]*typ[,,"other"]

gen <-  calcOutput("WasteGen", form="LogLog", aggregate=FALSE, pc=FALSE)[,,"SSP2"]
# total <- gen*shares
# total <- dimOrder(total,c(2,3,1))

#calibrate quantities based on shares vs nl
nl <- calcOutput("NlWasteDistrib", aggregate=F)
nl[,,"other"] <- nl[,,"other"] + nl[,,"wood"] + nl[,,"rubber"]
nl <- nl[,,c("wood","rubber"), invert=T]
nl <- round(nl,4)
nl[nl<0] <- 0
nl <- dimOrder(nl, c(2,1))

##
shares1 <- time_interpolate(shares, interpolated_year= getYears(nl), extrapolation_type = "linear")
regions <- intersect(getRegions(shares1), getRegions(nl))
years <- intersect(getYears(shares1), getYears(nl))
real <- nl[regions,years,]
c_factor <- dimSums(real[,,]/shares1[regions,,"Estimate.SSP2"], dim=2, na.rm=T)
c_factor[which(is.infinite(c_factor))] <- 1
c_factor[which(c_factor==0)] <- 1
shares[regions,,] <- collapseNames(setYears(c_factor,NULL)) * shares[regions,,]
#recalibrates everything back to 1
shares[regions,,] <- (1/dimSums(shares[regions,,"Estimate"],dim=3))*shares[regions,,]

total <- gen*shares
total<-dimOrder(total, c(3,4,1,2))


if (pc==TRUE){
  pop <- calcOutput("Population", aggregate=FALSE)
  getNames(pop) <- gsub("pop_","",getNames(pop))
  pop<- pop[,,SSP]
  total <- (total*10^9)/(pop*10^6)   #convert to kilos
 
   weight=pop[,,SSP]
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

