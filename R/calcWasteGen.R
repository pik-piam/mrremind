#' @title calcWasteGen
#' @description Calculates waste generation based on WhataWaste2.0 data, based on gdp regressions
#' and calibrated to real data multiplicatively
#' @param form Functional form of predicted waste generation
#' @param pc per capita (kg/capita) or total (Mt)
#' @author David Chen
#' @return magpie object of total waste generation
#' @importFrom magclass time_interpolate


calcWasteGen <- function(pc=TRUE, form="LogLog"){
  
gdppc <- calcOutput("GDPpc",aggregate=F)[,,c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")]

if(form=="LogLog"){
  pred <- 0.37*log(gdppc) + 2.16
  pred <- exp(pred)
}

if(form=="lm"){
  pred <- 0.0126290642*gdppc + 121.496
}

if(form=="MM"){
#based on MM function, bayesian process, pop-weighted
pred <- (1274.45 * gdppc)/(61542.95 + gdppc) + 92.15 
}
else if (form=="MM2"){
#based on MM2 function, bayesian process, pop-weighted
pred <- (927.55 * (gdppc^2))/(38358.16^2 + gdppc^2) + 163.55 
}

#multiplicative calibration
real <- readSource("Waste", "Generation")
tmp <- time_interpolate(pred,interpolated_year= getYears(real))
c_factor <- dimSums(real/tmp[,,"SSP2"], dim=2, na.rm=T)
#make no data ones 1
c_factor[which(c_factor==0)] <- 1
pred <- pred*collapseNames(setYears(c_factor,NULL))

pop <- calcOutput("Population", aggregate=F)[,,1:5]
getNames(pop) <- gsub("pop_","",getNames(pop))
if(pc == TRUE) {
  x = pred
  weight=pop
  unit="kg/cap"
} 
else {
  waste_totals <- collapseNames((pred*pop)/1000) 
  #1000 converts from million kg to millions of tons
  x<- waste_totals
  weight=NULL
  unit="t"
}
return(list(
  x=x,
  weight=weight,
  unit=unit,
  description="total waste generation"))
}

