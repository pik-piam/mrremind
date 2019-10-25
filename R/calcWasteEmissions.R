#' @title calcWasteEmissions
#' @description Calculates GHG emissions from solid waste disposal treatments, input from calcWasteProjections,  based on IPCC 2006 SWDS waste model and reporting calculations, in million t CO2eq
#' 
#' @param treatment type of waste treatment
#' @param yearly yearly or 5 year magpiesets
#' @return Magpie object of emissions from waste treatments
#' @author David Chen
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="WasteEmissions")
#' }
#' @importFrom magclass add_columns
#' @importFrom dplyr rename

calcWasteEmissions <- function(treatment="swds", yearly=FALSE){
  
  wastedistrib <- calcOutput("WasteProjections", pc=FALSE, aggregate=FALSE)
  
  if (treatment == "swds") {
     #CH4 from SWDS emissions from IPCC vol. 5 ch. 3
   #FOD equation
   # DDOCm =W * DOC * DOCf *MCF
   # DDOCm is mass of deposited degradable decomposable organic C <- ddoc_deposited
   # W weight; DOC defaults based on material type: org C for composting; 
   # DOCf decomposable fraction (default 0.5); 
   #MCF corr faction for aerobic decomp (based on disposal type)
   #MCF dumps 0.4 for shallow (taking worst case), but can be based on global avg? 
   ddoc_deposited<- wastedistrib[,,c("organic", "paper_cardboard", "wood_waste")] 
   ddoc_deposited <- ddoc_deposited[,,c("dumps", "landfills")]
   ddoc_deposited[,,"organic.landfills"] <- 0.2*0.5*1*ddoc_deposited[,,"organic.landfills"]
   ddoc_deposited[,,"paper_cardboard.landfills"]<- 0.4*0.5*1*ddoc_deposited[,,"paper_cardboard.landfills"]
   ddoc_deposited[,,"wood_waste.landfills"] <- 0.43*0.5*1*ddoc_deposited[,,"wood_waste.landfills"]
   ddoc_deposited[,,"organic.dumps"]<- 0.2*0.5*0.4*ddoc_deposited[,,"organic.dumps"]
   ddoc_deposited[,,"paper_cardboard.dumps"]<- 0.4*0.5*0.4*ddoc_deposited[,,"paper_cardboard.dumps"]
   ddoc_deposited[,,"wood_waste.dumps"] <- 0.43*0.5*0.4*ddoc_deposited[,,"wood_waste.dumps"]
 
   #add ddoc of landfills and dumps because all on carbon scale
   ddoc_deposited <- dimSums(ddoc_deposited, dim=3.2, na.rm=T)


  
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

# tmp <- ddoc_deposited[,"y2015",]
# tmp thing testing for constant past waste deposition value
#  for (i in 1965:2150){
#    ddoc_deposited[,i,] <- setYears(tmp,NULL)
#  }

ddoc_accumulated <- ddoc_deposited
ddoc_accumulated[] <- 0



#assuming accumulated at first time step is equal to amount deposited at that time step (pre-decomposition)??
ddoc_accumulated[,"y1965","paper_cardboard"] <- ddoc_deposited[,"y1965","paper_cardboard"]/(1-exp(-clim_k[,"y1965","kp"]))
ddoc_accumulated[,"y1965","organic"] <- ddoc_deposited[,"y1965","organic"]/(1-exp(-clim_k[,"y1965","ko"]))
ddoc_accumulated[,"y1965","wood_waste"] <- ddoc_deposited[,"y1965","wood_waste"]/(1-exp(-clim_k[,"y1965","kw"]))


#eqn 7 in IPCC spreadsheet model theory

for (i in (getYears(ddoc_accumulated[,-1,],as.integer=TRUE))){
    ddoc_accumulated[,i,"paper_cardboard"]  <- (setYears(ddoc_accumulated[,i-1,"paper_cardboard"], NULL)
                              + setYears(ddoc_deposited[,i,"paper_cardboard"],NULL)) *setYears(exp(-clim_k[,"y2005","kp"]), NULL)}

for (i in (getYears(ddoc_accumulated[,-1,],as.integer=TRUE))){
  ddoc_accumulated[,i,"organic"]  <- (setYears(ddoc_accumulated[,i-1,"organic"], NULL) 
  + setYears(ddoc_deposited[,i,"organic"],NULL))*setYears(exp(-clim_k[,"y2005","ko"]),NULL) }

for (i in (getYears(ddoc_accumulated[,-1,],as.integer=TRUE))){
  ddoc_accumulated[,i,"wood_waste"]  <- (setYears(ddoc_accumulated[,i-1,"wood_waste"],NULL) 
                                      + setYears(ddoc_deposited[,i,"wood_waste"],NULL))*setYears(exp(-clim_k[,"y2005","kw"]),NULL)}

#decomposed ddoc
ddoc_decomposed <- ddoc_deposited
ddoc_decomposed[] <-0

for (i in (getYears(ddoc_decomposed[,-1,],as.integer=TRUE))){
  ddoc_decomposed[,i,"paper_cardboard"]  <- (setYears(ddoc_accumulated[,i-1,"paper_cardboard"],NULL) 
                                           + setYears(ddoc_deposited[,i,"paper_cardboard"],NULL)) *setYears(1-exp(-clim_k[,"y2005","kp"]),NULL) }

for (i in (getYears(ddoc_decomposed[,-1,],as.integer=TRUE))){
  ddoc_decomposed[,i,"organic"]  <- (setYears(ddoc_accumulated[,i-1,"organic"],NULL) 
                                   + setYears(ddoc_deposited[,i,"organic"],NULL)) *setYears(1-exp(-clim_k[,"y2005","ko"]),NULL) }

for (i in (getYears(ddoc_decomposed[,-1,],as.integer=TRUE))){
  ddoc_decomposed[,i,"wood_waste"]  <- (setYears(ddoc_accumulated[,i-1,"wood_waste"],NULL) 
                                      + setYears(ddoc_deposited[,i,"wood_waste"],NULL)) *setYears((1-exp(-clim_k[,"y2005","kw"])),NULL) }



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
#in Mt  GHG 
    #IPCC differentiates emissiosn from biogenic waste and non-biogenic waste sources, 
    #here "other_comp" assumed to be non-bio
    #100000, 91700 kg GHG/TJ of material for co2
    #biomass MSW 11.6 TJ/Gg
    #non-biomass MSW 10 TJ/Gg
    #10^-6  for units
        
  incineration_org <- wastedistrib[,,c("organic.incineration", "paper_cardboard.incineration", 
                                       "wood_waste.incineration")]
  incineration_plas <- wastedistrib[,,c("plastic.incineration", "other.incineration", 
                                        "rubber_leather.incineration")]
  
  co2_org <- dimSums(incineration_org, dim=3.1)*10^-6*100000*11.6
  co2_plas <-dimSums(incineration_plas, dim=3.1)*10^-6*91700*10
    
  ch4_org <- dimSums(incineration_org, dim=3.1)*10^-6*30*11.6
  ch4_org <- 24*ch4_org
  ch4_plas <- dimSums(incineration_plas, dim=3.1)*10^-6*30*10
  ch4_plas <- 24*ch4_plas
  
  n2o_org <- dimSums(incineration_org, dim=3.1)*10^-6*4*11.6
  n2o_org <- 298*n2o_org
  n2o_plas <- dimSums(incineration_plas, dim=3.1)*10^-6*4*10
  n2o_plas <- 298*n2o_plas
  
  co2_non_bio <- ch4_org+ch4_plas+n2o_org+n2o_plas
  getNames(co2_non_bio, dim=1) <- "non_bio"
  
  getNames(co2_org, dim=1) <- "bio"
  
  out <- mbind(co2_non_bio, co2_org)
  out <- time_interpolate(out, interpolated_year= c(1965:2150), extrapolation_type = "linear")
  
  }

  
  if (yearly==FALSE){
    t<-findset("t_all")
    out <- out[,t,]
  }
  
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Co2eq",
    description="emissions from waste"))
}


#WITH IPCC TIME DELAY
# ddoc_decomposed_pap <- ddoc_notreacted_t[,,"paper_cardboard"]
# ddoc_decomposed[] <-0
# 
# for (i in (getYears(ddoc_decomposed[,-1,],as.integer=TRUE))){
#   ddoc_decomposed[,i,]  <- as.integer(ddoc_deposited[,i,] + as.integer(ddoc_accumulated[,i-1,]))
#   *(1-exp(-k)) 
#    }
# 
# following is from IPCC model with time delay; time delay not relevant for simplified FOD
# ddoc_decomposed <- ddoc_deposited 
#process starts with one year time delay ?
# M=13
# 
# ddoc_rem_pap <- ddoc_deposited[,,"paper_cardboard"]*exp(-kp*((13-M)/12))
# ddoc_decomposed_pap <- ddoc_deposited[,,"paper_cardboard"]*(1-exp(-kp*((13-M)/12)))
#  
# ddoc_rem_org <- ddoc_deposited[,,"organic"]*exp(-ko*((13-M)/12))
# ddoc_decomposed_org <- ddoc_deposited[,,"organic"]*(1-exp(-ko*((13-M)/12)))
# 
# ddoc_rem_wood <- ddoc_deposited[,,"wood_waste"]*exp(-kw*((13-M)/12))
# ddoc_decomposed_wood <- ddoc_deposited[,,"wood_waste"]*(1-exp(-kw*((13-M)/12)))
# 
# ddoc_notreacted <- mbind(ddoc_rem_pap,ddoc_rem_org, ddoc_rem_wood)
# ddoc_decomposed <-mbind(ddoc_decomposed_pap,ddoc_decomposed_org, ddoc_decomposed_wood)
 
 