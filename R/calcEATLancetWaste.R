#' @title calcEATLancetWaste
#' @description 
#' Calculates the ratio between food supply at household level and food intake for different MAgPIE commodities. 
#'
#' @param out_type ratio: total food supply to totoal intake. 
#' ratio_detailed: food-specific estimates. 
#' ratio_detailed_FAO: food-specific estimates based on FAO food waste shares 
#' calib: factor for calibrating estimates based on FAO waste shares to food supply
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' 
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{readEATLancet}},
#' \code{\link{calcEATLancetDiets}}, \code{\link{convertEATLancet}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EATLancetWaste")
#' }
#' @export

calcEATLancetWaste <- function(out_type="ratio"){
  
  fsupply.hist <- calcOutput(type = "FoodSupplyPast", aggregate = FALSE, per_capita = TRUE, product_aggr = FALSE, attributes = "kcal")[,"y2010",]
  getSets(fsupply.hist)[3] <- "kfo"
  
  Mag_Intake <- calcOutput("Intake",modelinput="TRUE", standardize=FALSE, method="FAO_WHO_UNU1985", aggregate=FALSE)
  Mag_Intake <- collapseNames(Mag_Intake[,"y2010","SSP2"])
  
  Mag_EAT_diets <- calcOutput(type = "EATLancetDiets", aggregate = FALSE, attributes = c("kcal","wm"), calib = TRUE, FAOcountr = TRUE)
  Mag_EAT_diets <- collapseNames(Mag_EAT_diets[,"y2010","BMK"][,,"2100kcal"][,,"wm",invert=TRUE])
  
  Intake_calib <- Mag_Intake/dimSums(Mag_EAT_diets,dim=3)
  Intake_calib[which(!is.finite(Intake_calib))] <- 1

  Mag_EAT_diets <- Mag_EAT_diets*Intake_calib
  
  
  #### Calculation of the ratio between food supply at household level and food intake
  #based on FAO estimates on food waste at consumption level and food conversion factors
  
  FAO_waste_shr <- readSource(type="FAOLossesWaste",subtype="Consumption")
  
  # mapping of FAO waste categories to MAgPIE food commodities
  Mag_kfo <- findset("kfo")
  
  FAO_wgroups <- c("Oilseeds and pulses","Cereals","Roots and tubers","Meat","Oilseeds and pulses","Meat","Milk","Milk","Meat","Meat",
            "Cereals","Fruits and vegetables","Oilseeds and pulses","Fruits and vegetables","Roots and tubers","Oilseeds and pulses","Oilseeds and pulses","Cereals",
            "Oilseeds and pulses","Oilseeds and pulses","Oilseeds and pulses","Fruits and vegetables","Fruits and vegetables","Oilseeds and pulses","Cereals","Cereals")
  rel_matrix <- cbind(Mag_kfo,FAO_wgroups)
  
  FAO_waste_shr_detailed<-toolAggregate(FAO_waste_shr,rel = rel_matrix,
                                          dim = 3,from = "FAO_wgroups",to = "Mag_kfo", partrel=FALSE)
  
  #Conversion factors into edible matter: 0.82 for roots, 0.79 for maize, 0.78 for wheat, 1 for rice, 
  #0.78 for other grains, 0.77 for fruits and vegetables, 1 for meat, 1 for oilseeds and pulses, 1 for milk
  conv_fact <- dimSums(FAO_waste_shr,dim=1)
  conv_fact[,,"Cereals"] <- 0.78
  conv_fact[,,"Roots and tubers"] <- 0.82
  conv_fact[,,"Oilseeds and pulses"] <- 1
  conv_fact[,,"Fruits and vegetables"] <- 0.77
  conv_fact[,,"Meat"] <- 1
  conv_fact[,,"Milk"] <- 1
  
  conv_fact_detailed<-toolAggregate(conv_fact,rel = rel_matrix,
                                        dim = 3,from = "FAO_wgroups",to = "Mag_kfo", partrel=FALSE)
  
  conv_fact_detailed[,,"brans"] <- 1
  conv_fact_detailed[,,"maiz"] <- 0.79
  conv_fact_detailed[,,"rice_pro"] <- 1

  
  #calculation of the share of wasted food at household level including losses from food conversion into edible matter
  HH_waste_shr_detailed <- FAO_waste_shr_detailed + (1-FAO_waste_shr_detailed)*(1-conv_fact_detailed)
  fsupply_estimated <- Mag_EAT_diets/(1-HH_waste_shr_detailed)
  overcons_FAO <- fsupply_estimated/Mag_EAT_diets
  if(any(!is.finite(overcons_FAO)) ){
    temp_overcons <- overcons_FAO
    temp_overcons[which(!is.finite(temp_overcons))] <- 1
    replacement <- as.magpie(apply(temp_overcons,3,mean, na.rm=TRUE))
    overcons_FAO <- collapseNames(toolNAreplace(overcons_FAO, replaceby=replacement)$x)
  }
  
  fsupply_calib <-  dimSums(fsupply.hist,dim=3)/dimSums(fsupply_estimated,dim=3)
  fsupply_calib[which(!is.finite(fsupply_calib))] <- 1
  
  overcons_estimated <- fsupply_estimated*fsupply_calib/Mag_EAT_diets
  if(any(!is.finite(overcons_estimated)) ){
    temp_overcons <- overcons_estimated
    temp_overcons[which(!is.finite(temp_overcons))] <- 1
    replacement <- as.magpie(apply(temp_overcons,3,mean, na.rm=TRUE))
    overcons_estimated <- collapseNames(toolNAreplace(overcons_estimated, replaceby=replacement)$x)
  }
  
  Mag_overcons_fctr <- dimSums(fsupply.hist,dim=3)/Mag_Intake
  if(min(Mag_overcons_fctr)==0){
    temp_overcons <- Mag_overcons_fctr
    temp_overcons[which(Mag_overcons_fctr==0)] <- 1
    replacement <- as.magpie(apply(temp_overcons,3,mean, na.rm=TRUE))
    Mag_overcons_fctr[which(Mag_overcons_fctr==0)] <- replacement
  }
  
  if(out_type=="ratio") {
    data.out <- Mag_overcons_fctr
    description = "ratio between total food calorie supply and total food calorie intake"
  } else if(out_type=="ratio_detailed") {
    data.out <- overcons_estimated
    description = "food-specific ratio between food calorie supply and food intake"
  } else if(out_type=="ratio_detailed_FAO") {
    data.out <- overcons_FAO
    description = "food-specific ratio between food calorie supply and food intake based on FAO food waste shares"    
  } else if (out_type=="calib") {
    data.out <-  fsupply_calib 
    description = "factor for calibrating estimated food supply (based on intake and FAO waste shares) to FAO food supply"
  } else {stop("unknown type")}
  
  data.out <- setYears(data.out,NULL)
  
  #### Define weights and units
  
  weight.pop <- collapseNames(calcOutput("Population",aggregate = FALSE)[,"y2010","pop_SSP2"])
  unit <- "-"
  
  return(list(x=data.out,
              weight=weight.pop,
              unit=unit,
              description=description)
  )
}
