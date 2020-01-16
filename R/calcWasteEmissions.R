#' @title calcWasteEmissions
#' @description Calculates GHG emissions from solid waste disposal treatments, input from calcWasteProjections,  based on IPCC 2006 SWDS waste model and reporting calculations, in million t CO2eq
#' @param treatment type of waste treatment
#' @return Magpie object of emissions from waste treatments
#' @author David Chen
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="WasteEmissions")
#' }
#' @importFrom magclass add_columns
#' @importFrom dplyr rename

calcWasteEmissions <- function(treatment="swds"){
  
  wastedistrib <- calcOutput("WasteProj", pc=FALSE, aggregate=FALSE)
  
  if (treatment == "swds") {
     #CH4 from SWDS emissions from IPCC vol. 5 ch. 3
   #FOD equation
   # DDOCm =W * DOC * DOCf *MCF
    # DDOCm is mass of deposited degradable decomposable organic C <- ddoc_deposited
    # W weight; DOC defaults based on material type: org C for composting; 
    # DOCf decomposable fraction when anaerobic (2019 refinement); 
    #MCF corr faction for aerobic decomp (based on disposal type)
    #MCF dumps 0.4 for shallow (taking worst case), but can be based on global avg? 
    ddoc_deposited<- wastedistrib[,,c("organic", "paper")] 
    ddoc_deposited <- ddoc_deposited[,,c("dumps", "landfills")]
    ddoc_deposited[,,"organic.landfills"] <- 0.2*0.7*1*ddoc_deposited[,,"organic.landfills"]
    ddoc_deposited[,,"paper.landfills"]<- 0.4*0.5*1*ddoc_deposited[,,"paper.landfills"]
    #ddoc_deposited[,,"wood_waste.landfills"] <- 0.43*0.1*1*ddoc_deposited[,,"wood_waste.landfills"]
    ddoc_deposited[,,"organic.dumps"]<- 0.2*0.7*0.4*ddoc_deposited[,,"organic.dumps"]
    ddoc_deposited[,,"paper.dumps"]<- 0.4*0.5*0.4*ddoc_deposited[,,"paper.dumps"]
    #ddoc_deposited[,,"wood_waste.dumps"] <- 0.43*0.1*0.4*ddoc_deposited[,,"wood_waste.dumps"]
    
    #add ddoc of landfills and dumps because all on carbon scale
    #ddoc_deposited <- dimSums(ddoc_deposited, dim=3.2, na.rm=T)
    
    #Climate region default k values, but boreal moist dry currently set to temperate moist/dry values since not given:
    #kp temperate dry: 0.04, temp wet:0.06, tropical dry:0.045, trop wet: 0.07
    #ko temperate dry: 0.055, temp wet: 0.1425, trop dry: 0.075, trop wet: 0.285
    #kw temperate dry: 0.02, temp wet: 0.03, trop dry: 0.025, trop wet: 0.035
    clim_reg <- calcOutput("IPCCClimateRegions",aggregate=FALSE, yearly=TRUE, landusetypes="urban") 
    #dirty fix for these countries which have 0 climate somehow
    clim_reg[c("ATF","SGS", "MUS", "VUT","REU"),,"tropical_wet"] <- 1
    
    kp <- c(0.04, 0.06, 0.045, 0.07)
    ko <- c(0.055, 0.1425, 0.075, 0.285)
    kw <- c(0.02, 0.03, 0.025, 0.035)
    
    row_names<- c("warm_temp_dry", "warm_temp_moist","tropical_dry","tropical_moist")
    ef_table <- data.frame(kp,ko,kw, row.names = row_names)
    ef_table <- rbind(ef_table, "tropical_wet" = c(0.07,0.285,0.035))
    ef_table <- rbind(ef_table, "cool_temp_moist" = c(0.06, 0.1425, 0.03))
    ef_table <- rbind(ef_table, "cool_temp_dry" = c(0.04, 0.0550, 0.02))
    ef_table <- rbind(ef_table, "boreal_moist" = c(0.06,0.1425,0.03))
    ef_table <- rbind(ef_table, "boreal_dry" = c(0.04,0.055,0.02))
    ef_table <- rbind(ef_table, "polar_moist" = c(0.04,0.055,0.02))
    ef_table <- rbind(ef_table, "polar_dry" = c(0.04,0.055,0.02))
    
    # dimension climate wastetype
    
    k <- as.magpie(ef_table, dimNames=c("climate","k"))
    
    clim_k = (clim_reg*k)
    clim_k <- dimSums(clim_k, dim=3.1)
    
    #clim_k doesn't have future values so calculations are not dynamic with changing k based on changing climate, set to current climate
    
    ddoc_deposited <- time_interpolate(ddoc_deposited, interpolated_year= c(1965:2150), extrapolation_type = "linear")
    
    #accumulated stock
    ddoc_accumulated <- ddoc_deposited
    ddoc_accumulated[] <- 0
    
    #assuming accumulated at first time step is equal to amount deposited at that time step (pre-decomposition)??
    ddoc_accumulated[,"y1965","paper"] <- ddoc_deposited[,"y1965","paper"]/(1-exp(-clim_k[,"y1965","kp"]))
    ddoc_accumulated[,"y1965","organic"] <- ddoc_deposited[,"y1965","organic"]/(1-exp(-clim_k[,"y1965","ko"]))
    #ddoc_accumulated[,"y1965","wood_waste"] <- ddoc_deposited[,"y1965","wood_waste"]/(1-exp(-clim_k[,"y1965","kw"]))
    
    
    #ddoc_rem is amount of deposited waste remaining at time t
    ddoc_rem <- ddoc_deposited
    ddoc_rem[] <- 0
    
    #ddoc_dec is amount decomposed from the deposited amount
    ddoc_dec <- ddoc_deposited
    ddoc_dec[] <- 0
    
    #ddoc_decomposed total amount decomposed
    ddoc_decomposed <- ddoc_deposited
    ddoc_decomposed[] <-0
    
    
    t <- getYears(ddoc_accumulated[,-1,],as.integer=TRUE)
    #eqn 7 in IPCC spreadsheet model 
    
    for (i in t){
      ddoc_rem[,i,"paper"]  <- setYears(ddoc_deposited[,i,"paper"],NULL)*setYears(exp(-clim_k[,"y2005","kp"]), NULL)}
    
    for (i in t){
      ddoc_rem[,i,"organic"]  <- setYears(ddoc_deposited[,i,"organic"],NULL)*setYears(exp(-clim_k[,"y2005","ko"]), NULL)}
    
    
    for (i in t){
      ddoc_dec[,i,"paper"]  <- setYears(ddoc_deposited[,i,"paper"],NULL)*setYears(1-exp(-clim_k[,"y2005","kp"]), NULL)}
    
    
    for (i in t){
      ddoc_dec[,i,"organic"]  <- setYears(ddoc_deposited[,i,"organic"],NULL)*setYears(1-exp(-clim_k[,"y2005","ko"]), NULL)}
    
    for (i in t){
      ddoc_accumulated[,i,"paper"]  <- (setYears(ddoc_rem[,i,"paper"], NULL)
                                        + setYears(ddoc_accumulated[,i-1,"paper"],NULL)) *setYears(exp(-clim_k[,"y2005","kp"]), NULL)}
    
    for (i in t){
      ddoc_accumulated[,i,"organic"]  <- (setYears(ddoc_rem[,i,"organic"], NULL) 
                                          + setYears(ddoc_accumulated[,i-1,"organic"],NULL))*setYears(exp(-clim_k[,"y2005","ko"]),NULL) }
    
    # for (i in (getYears(ddoc_accumulated[,-1,],as.integer=TRUE))){
    #   ddoc_accumulated[,i,"wood_waste"]  <- (setYears(ddoc_accumulated[,i-1,"wood_waste"],NULL) 
    #                                       + setYears(ddoc_deposited[,i,"wood_waste"],NULL))*setYears(exp(-clim_k[,"y2005","kw"]),NULL)}
    
    
    for (i in t){
      ddoc_decomposed[,i,"paper"]  <- (setYears(ddoc_dec[,i,"paper"], NULL)
                                       + setYears(ddoc_accumulated[,i-1,"paper"],NULL)*setYears(1-exp(-clim_k[,"y2005","kp"]),NULL))
    }
    
    for (i in t){
      ddoc_decomposed[,i,"organic"]  <- (setYears(ddoc_dec[,i,"organic"],NULL)
                                         +setYears(ddoc_accumulated[,i-1,"organic"],NULL)*setYears(1-exp(-clim_k[,"y2005","ko"]),NULL))
    }
    # 
    # for (i in (getYears(ddoc_decomposed[,-1,],as.integer=TRUE))){
    #   ddoc_decomposed[,i,"wood_waste"]  <- (setYears(ddoc_accumulated[,i-1,"wood_waste"],NULL) 
    #                                       + setYears(ddoc_deposited[,i,"wood_waste"],NULL)) *setYears((1-exp(-clim_k[,"y2005","kw"])),NULL) }
    # 
    
  #16/12 molecular mass
  #DDOC_dcomposed *16/12 * F
  #F=0.5, fraction of CH4 in gas, other amount is CO2
  ch4_swds<- ddoc_decomposed * 16/12 * 0.5
  #2 more factors for recovery and oxidation defaults don't change 
  #ch4_paper <- (ch4_paper-R(t)) * (1-Ox) 
  R=0
  Ox = 0
  ch4_swds <- (ch4_swds-R) * (1-Ox) 
    
    co2e_swds <- ch4_swds*24
    
    out<-co2e_swds

}

  else if (treatment == "compost") {
    #from IPCC ch. 5
    #CH4emissions  = Mass of organic waste*emissions factor(gCH4/kg)*10^-3 - R 
    #emissions factor EF 4 for wet weight compost ch4, 0.24 for wet weight compost n2o
    #R is recovery amount, process-dependent, assumed 0
    #Mt = 1000 Gg, cancels out with emissions factor
    #ch4 to co2eq 24:1
    #n2o to co2e 298:1
compost <- wastedistrib[,,"compost"]
ch4_compost <- compost*4*(10^-3)
ch4_compost <- time_interpolate(ch4_compost, interpolated_year= c(1965:2150), extrapolation_type = "linear")
ch4_compost <- ch4_compost*24


n2o_compost <- compost*0.24*(10^-3)
n2o_compost <- time_interpolate(n2o_compost, interpolated_year= c(1965:2150), extrapolation_type = "linear")
n2o_compost <- n2o_compost*298

ghg_compost <-mbind(ch4_compost,n2o_compost)

out<- ghg_compost  

}

 else if (treatment == "wte") {
#IPCC Vol5ch5
   #assum wet weight
#CO2:
   #plastic 75% carbon content, 100% is fossil
   #food waste 38% carbon content, 0 fossil
   #paper waste 40% carbon content, 0 fossil
#CH4: 0 default emissions factor, open burning unknown/not assessed
   
#N2O: default of 55g/tMSW incinerated for all waste

   
   
   
   
        
  incineration<- wastedistrib[,,c("paper","plastic","organic")][,,"incineration"]

  co2_org <- incineration[,,"organic"]*0.15 *44/12
  co2_pap <- incineration[,,"paper"]*0.40*44/12
  co2_plas <-incineration[,,"plastic"]*0.75*44/12
  
  n2o <- incineration*55*10^-6
  n2o <- 298*n2o
  getNames(n2o,dim=1) <-paste0("n2o_",getNames(n2o,dim=1))

  out <- mbind(co2_org, co2_pap, co2_plas, n2o)
  out <- time_interpolate(out, interpolated_year= c(1965:2150), extrapolation_type = "linear")
  
  }
  
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Co2eq",
    description="emissions from waste"))
}



 