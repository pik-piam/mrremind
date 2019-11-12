#' @title calcWasteProjections
#' 
#' @description Creates waste projections for composition and treatment 
#' @param pc MAgpIE object containing Waste data on country level, total(million tons) or per capita (kg/capita)
#' @return Waste data as complete MAgPIE object on country level
#' @author David Chen
#' @seealso \code{\link{readSource}}
# @importFrom mrregression toolRegression


calcWasteProjections <- function(pc=TRUE){
  
  gdp_pc <- calcOutput("GDPpc", aggregate=FALSE)
  
#generation totals
  
  food<-(159*gdp_pc)/(2.405e+03+gdp_pc)
  yard <- (2.056e-03*gdp_pc)
  organic <- ((198.1*gdp_pc)/(4.58e+03+gdp_pc))
  glass <- (8.725e-04*gdp_pc)
  metal <- (9.941e-04*gdp_pc)
  paper_cardboard <- (3.779e-03*gdp_pc)
  wood_waste <- (8.185e-04*gdp_pc)
  plastic <- (2.076e-03*gdp_pc)
  rubber_leather <- (1.104e-03*gdp_pc)
  other <- (1.493*gdp_pc)^0.3803
  
  food_yard_compost = (gdp_pc^2)/(4.221e+04^2+gdp_pc^2)
  food_yard_inci = (exp(-1.508*exp(-1.6e-05*gdp_pc)))
  food_yard_compost_inci = ((gdp_pc)^2)/(2.902e+04^2+(gdp_pc)^2) #alternate
  food_yard_dumps= (exp((-gdp_pc/9.299e+03)))
   
  glass_metal_recyc = (gdp_pc^2)/((4.029e+04^2+gdp_pc^2))
  glass_metal_dumps = exp((-gdp_pc/9.115e+03))
  
  pap_wood_recyc = ((gdp_pc))/(8.75e+04+(gdp_pc))
  #pap_wood_compost =((gdp_pc)^2)/(2.902e+04^2+(gdp_pc)^2) 
  pap_wood_inci = (gdp_pc^2)/(3.181e+04^2+gdp_pc^2) #not so great and same as org and plastic??
  # pap_wood_compost_inci = ((gdp_pc)^2)/(2.902e+04^2+(gdp_pc)^2) #alternate
  pap_wood_dumps = exp((-gdp_pc/9.115e+03))
  
  plas_rubb_other_recyc = ((gdp_pc))/(1.624e+05+(gdp_pc))
  plas_rubb_other_inci = ((gdp_pc)^2)/(3.181e+04^2+(gdp_pc)^2) #this one not great
  plas_rubb_other_recyc_inci = ((gdp_pc)^2)/(2.979e+04^2+(gdp_pc)^2) #alternate
  plas_rubb_other_dumps = exp((-gdp_pc/9.13e+03))
  
  
  organic_compost <- organic*food_yard_compost
  organic_inci <- (organic-organic_compost)*food_yard_inci
  organic_dumps <- (organic-organic_compost-organic_inci)*food_yard_dumps
  organic_landfills <- (organic-organic_compost-organic_inci)*(1-food_yard_dumps)
  
  getNames(organic_compost) <- paste0("organic.compost.", getNames(organic_compost))
  getNames(organic_inci) <- paste0("organic.incineration.", getNames(organic_inci))
  getNames(organic_dumps) <- paste0("organic.dumps.", getNames(organic_dumps))
  getNames(organic_landfills) <- paste0("organic.landfills.", getNames(organic_landfills))
  
  organic <- mbind(organic_compost, organic_inci, organic_dumps, organic_landfills)
  organic <- add_columns(organic, addnm=c("recycling"), dim=3.2)
  
  glass_recycling <- glass*glass_metal_recyc
  glass_dumps <- (glass-glass_recycling)*glass_metal_dumps
  glass_landfills <- (glass-glass_recycling)*(1-glass_metal_dumps)
  
  getNames(glass_recycling) <- paste0("glass.recycling.", getNames(glass_recycling))
  getNames(glass_dumps) <- paste0("glass.dumps.", getNames(glass_dumps))
  getNames(glass_landfills) <- paste0("glass.landfills.", getNames(glass_landfills))
  
  glass <- mbind(glass_recycling, glass_dumps, glass_landfills)
  glass <- add_columns(glass, addnm=c("compost", "incineration"), dim=3.2)
  
  
  metal_recycling <- metal*glass_metal_recyc
  metal_dumps <- (metal-metal_recycling)*glass_metal_dumps
  metal_landfills <- (metal-metal_recycling)*(1-glass_metal_dumps)
  getNames(metal_recycling) <- paste0("metal.recycling.", getNames(metal_recycling))
  getNames(metal_dumps) <- paste0("metal.dumps.", getNames(metal_dumps))
  getNames(metal_landfills) <- paste0("metal.landfills.", getNames(metal_landfills))
  
  metal <- mbind(metal_recycling, metal_dumps, metal_landfills)
  metal <- add_columns(metal, addnm=c("compost", "incineration"), dim=3.2)
  
  
  paper_recycling <- paper_cardboard*pap_wood_recyc
  paper_incineration <- (paper_cardboard-paper_recycling)*pap_wood_inci
  paper_dumps <- (paper_cardboard-paper_recycling-paper_incineration)*pap_wood_dumps
  paper_landfills <-(paper_cardboard-paper_recycling-paper_incineration)*(1-pap_wood_dumps)
  getNames(paper_recycling) <- paste0("paper_cardboard.recycling.", getNames(paper_recycling))
  getNames(paper_incineration) <-  paste0("paper_cardboard.incineration.", getNames(paper_incineration))
  getNames(paper_dumps) <-  paste0("paper_cardboard.dumps.", getNames(paper_dumps))
  getNames(paper_landfills) <-  paste0("paper_cardboard.landfills.", getNames(paper_landfills))
  
  paper_cardboard <- mbind(paper_recycling,paper_incineration, paper_dumps,paper_landfills )
  paper_cardboard <- add_columns(paper_cardboard, addnm=c("compost"), dim=3.2)
  
  wood_recycling <- wood_waste*pap_wood_recyc
  wood_incineration <- (wood_waste-wood_recycling)*pap_wood_inci
  wood_dumps <- (wood_waste-wood_recycling-wood_incineration)*pap_wood_dumps
  wood_landfills <-(wood_waste-wood_recycling-wood_incineration)*(1-pap_wood_dumps)
  getNames(wood_recycling) <-  paste0("wood_waste.recycling.", getNames(wood_recycling))
  getNames(wood_incineration) <- paste0("wood_waste.incineration.", getNames(wood_incineration))
  getNames(wood_dumps) <-  paste0("wood_waste.dumps.", getNames(wood_dumps))
  getNames(wood_landfills) <-  paste0("wood_waste.landfills.", getNames(wood_landfills))
  
  wood_waste <- mbind(wood_recycling,wood_incineration, wood_dumps,wood_landfills)
  wood_waste <- add_columns(wood_waste, addnm=c("compost"), dim=3.2)
  
  plastic_recycling <- plastic*plas_rubb_other_recyc
  plastic_incineration <- (plastic-plastic_recycling)*plas_rubb_other_inci
  plastic_dumps <- (plastic-plastic_recycling-plastic_incineration)*plas_rubb_other_dumps
  plastic_landfills <-(plastic-plastic_recycling-plastic_incineration)*(1-plas_rubb_other_dumps)
  getNames(plastic_recycling) <- paste0("plastic.recycling.", getNames(plastic_recycling))
  getNames(plastic_incineration) <- paste0("plastic.incineration.", getNames(plastic_incineration))
  getNames(plastic_dumps) <-  paste0("plastic.dumps.", getNames(plastic_dumps))
  getNames(plastic_landfills) <-  paste0("plastic.landfills.", getNames(plastic_landfills))
  
  plastic <- mbind(plastic_recycling,plastic_incineration, plastic_dumps,plastic_landfills)
  plastic <- add_columns(plastic, addnm=c("compost"), dim=3.2)
  
  
  rubber_leather_recycling <- rubber_leather*plas_rubb_other_recyc
  rubber_leather_incineration <- (rubber_leather-rubber_leather_recycling)*plas_rubb_other_inci
  rubber_leather_dumps <- (rubber_leather-rubber_leather_recycling-rubber_leather_incineration)*plas_rubb_other_dumps
  rubber_leather_landfills <-(rubber_leather-rubber_leather_recycling-rubber_leather_incineration)*(1-plas_rubb_other_dumps)
  getNames(rubber_leather_recycling) <-  paste0("rubber_leather.recycling.", getNames(rubber_leather_recycling))
  getNames(rubber_leather_incineration) <-  paste0("rubber_leather.incineration.", getNames(rubber_leather_incineration))
  getNames(rubber_leather_dumps) <-  paste0("rubber_leather.dumps.", getNames(rubber_leather_dumps))
  getNames(rubber_leather_landfills) <-  paste0("rubber_leather.landfills.", getNames(rubber_leather_landfills))
  
  rubber_leather <- mbind(rubber_leather_recycling,rubber_leather_incineration, rubber_leather_dumps,rubber_leather_landfills)
  rubber_leather <- add_columns(rubber_leather, addnm=c("compost"), dim=3.2)
  
  
  other_recycling <- other*plas_rubb_other_recyc
  other_incineration <- (other-other_recycling)*plas_rubb_other_inci
  other_dumps <- (other-other_recycling-other_incineration)*plas_rubb_other_dumps
  other_landfills <-(other-other_recycling-other_incineration)*(1-plas_rubb_other_dumps)
  getNames(other_recycling) <-  paste0("other.recycling.", getNames(other_recycling))
  getNames(other_incineration) <-  paste0("other.incineration.", getNames(other_incineration))
  getNames(other_dumps) <- paste0("other.dumps.", getNames(other_dumps))
  getNames(other_landfills) <-  paste0("other.landfills.", getNames(other_landfills))
  
  other <- mbind(other_recycling,other_incineration, other_dumps,other_landfills)
  other <- add_columns(other, addnm=c("compost"), dim=3.2)
  
  
  x <- mbind(organic, glass, metal, wood_waste, paper_cardboard, other, plastic, rubber_leather)
  
  x_tmp<- collapseNames(x[,,"SSP2"])
  
  #calibrate x
  a<- calcOutput("WasteDistrib", aggregate=FALSE)
  gen <- readSource("Waste", subtype="Generation", convert=TRUE)
  
  
  landfills <- a[,,c("landfill_unspecified", "controlled_landfill", 
                     "sanitary_landfill_landfill_gas_system")] 
  landfills <- dimSums(landfills, dim=3.2, na.rm=T)
  landfills <- add_dimension(landfills, dim=3.2, nm="landfills")
  
  a <- a[,,c("compost", "incineration", "dumps","recycling")]
  a <- mbind(a, landfills)

  a<-gen*a #now on quantity basis
  
  a<- collapseNames(a)

  # 
  # x <- x[,,c("compost", "incineration", "dumps", "landfills", "recycling")]
  # x <- x[,,c("organic","glass","metal","wood_waste","paper_cardboard","other","plastic",
  #            "rubber_leather")]
  #    
  # when neither composition nor treatment exists, the NA turns into 1 in calibration, giving
  #a value for treatments that don't exist ONLY for the compositions that ALSO don't exist
  #this sets these cases to 0
  
  # for (i in c("compost", "incineration", "dumps", "landfills", "recycling")){
  #   x[which(dimSums(a[,,i], dim=3.1, na.rm=T) == 0),,i] <- 0}
    
  calibration_factor = a[,"y2015",]-x_tmp[,"y2015",]
  calibration_factor[is.na(calibration_factor)] = 0
  calibrated = x + setYears(calibration_factor,NULL)
  calibrated[calibrated<0] = 0
  # 
 
  # when neither composition nor treatment exists, the NA turns into 1 in calibration, giving
  #a value for treatments that don't exist ONLY for the compositions that ALSO don't exist
  #this sets these cases to 0

 
  #   
  # calibration_factor <- a/x[,"y2015","SSP2"]
  # calibration_factor[is.na(calibration_factor)] = 1
  # #calibration_factor[calibration_factor == 0] =0
  # calibrated = x * setYears(calibration_factor,NULL)
  # calibrated[calibrated<0] = 0

  waste_pc <- collapseNames(calibrated)
  pop<-calcOutput("Population", aggregate=F)
  getNames(pop) <- gsub("pop_", "pop.", getNames(pop))
  
  
  
  
  
  if(pc == TRUE) {
    x = waste_pc
    weight=pop
    unit="kg/cap"
  } else {
    waste_totals <- collapseNames((waste_pc*pop)/1000) #1000 converts from million kg to millions of tons
    x<- waste_totals
    weight=NULL
    unit="t"
  }
  
  x[is.na(x)] <- 0
  x[c("LIE","MCO", "CYM", "MAC"),,] <- 0

return(list(
  x=x,
  weight=weight,
  unit=unit,
  description="Waste Projections based on What a Waste 2.0 dataset in million tons or kg/cap"))
}




  # old waste data
  # paper_recycled_and_wte = ((gdp_pc)^2)/((3.248+04)^2+(gdp_pc)^2)
  # paper_dumps_over_landfills = (4.162e+03)/((6.039e+03)+(gdp_pc))
  # plastic_recycled_and_wte = ((gdp_pc)^2)/((3.248+04)^2+(gdp_pc)^2)
  # plastic_dumps_over_landfills = (4.162e+03)/((6.039e+03)+(gdp_pc))
  # generation_pc_reg = (0.9478*gdp_pc)/(8.561e+03 +(gdp_pc))
  
#   
#   calibration <-function(numerator,denominator,regression) {
#     top <-collapseNames(numerator)
#     bottom <- collapseNames(denominator)
#     percentage <- collapseNames(top/bottom)
#     calibration_factor = percentage/regression[,"y2015",]
#     calibration_factor[is.na(calibration_factor)] = 1
#     calibration_factor[calibration_factor == 0] =1
#     calibrated = regression * setYears(calibration_factor,NULL)
#     calibrated[calibrated<0] = 0
#     calibrated[calibrated>1] = 1
#     a<-collapseNames(calibrated)
#     return(a)
#     
#   }
#   
#   food_yard_compost_calib <- calibration(numerator = (x[,"y2015","organic.compost"]),
#                                            denominator =(x[,"y2015","organic.compost"]+
#                                                          x[,"y2015","organic.dumps"]+
#                                                          x[,"y2015","organic.landfill_unspecified"]+
#                                                          x[,"y2015","organic.incineration"]+
#                                                          x[,"y2015","organic.sanitary_landfill_landfill_gas_system"]+
#                                                          x[,"y2015","organic.controlled_landfill"]),
#                                            regression= food_yard_compost)
#   
#   food_yard_inci_calib <-  calibration(a, numerator = (x[,"y2015","organic.incineration"]),
#                                        denominator =(x[,"y2015","organic.dumps"]+
#                                                        x[,"y2015","organic.landfill_unspecified"]+
#                                                        x[,"y2015","organic.incineration"]+
#                                                        x[,"y2015","organic.sanitary_landfill_landfill_gas_system"]+
#                                                        x[,"y2015","organic.controlled_landfill"]),
#                                                  regression= food_yard_inci)
#   #alternative
#   food_yard_compost_inci_calib <-  calibration(a,numerator = (x[,"y2015","organic.incineration"]+x[,"y2015","organic.compost"]),
#                                                denominator =(x[,"y2015","organic.compost"]+
#                                                               x[,"y2015","organic.dumps"]+
#                                                                x[,"y2015","organic.landfill_unspecified"]+
#                                                                x[,"y2015","organic.incineration"]+
#                                                                x[,"y2015","organic.sanitary_landfill_landfill_gas_system"]+
#                                                                x[,"y2015","organic.controlled_landfill"]),
#                                        regression= food_yard_compost_inci)
# 
#   food_yard_dumps_calib <-  calibration(a,  numerator = (x[,"y2015","organic.incineration"]),
#                                         denominator =(x[,"y2015","organic.dumps"]+
#                                                         x[,"y2015","organic.landfill_unspecified"]+
#                                                         x[,"y2015","organic.sanitary_landfill_landfill_gas_system"]+
#                                                         x[,"y2015","organic.controlled_landfill"]),
#                                                         regression= food_yard_dumps)
#   
#    glassmetal_recyc_calib <-calibration(a, numerator = (x[,"y2015","glass.recycling"]+x[,"y2015","metal.recycling"]),
#                                           denominator =(x[,"y2015","glass.controlled_landfill"]+x[,"y2015","metal.controlled_landfill"]+
#                                                         x[,"y2015","glass.landfill_unspecified"]+x[,"y2015","metal.landfill_unspecified"]+
#                                                         x[,"y2015","glass.sanitary_landfill_landfill_gas_system"]+
#                                                         x[,"y2015","metal.sanitary_landfill_landfill_gas_system"]+
#                                                         x[,"y2015","glass.recycling"]+x[,"y2015","metal.recycling"]+
#                                                         x[,"y2015","glass.dumps"]+x[,"y2015","metal.dumps"]),
#                                           regression= glass_metal_recyc)
#    
#    glassmetal_dumps_calib <-calibration(a, numerator = (x[,"y2015","glass.dumps"]+x[,"y2015","metal.dumps"]),
#                                         denominator =(x[,"y2015","glass.controlled_landfill"]+x[,"y2015","metal.controlled_landfill"]+
#                                                         x[,"y2015","glass.landfill_unspecified"]+x[,"y2015","metal.landfill_unspecified"]+
#                                                         x[,"y2015","glass.sanitary_landfill_landfill_gas_system"]+
#                                                         x[,"y2015","metal.sanitary_landfill_landfill_gas_system"]+
#                                                         x[,"y2015","glass.dumps"]+x[,"y2015","metal.dumps"]),
#                                         regression=glass_metal_dumps)
#    
#   pap_wood_recyc_calib <- calibration(a, numerator = (x[,"y2015","paper_cardboard.recycling"]+x[,"y2015","wood_waste.recycling"]),
#                                 denominator =(x[,"y2015","paper_cardboard.controlled_landfill"]+x[,"y2015","wood_waste.controlled_landfill"]+
#                                                 x[,"y2015","paper_cardboard.landfill_unspecified"]+x[,"y2015","wood_waste.landfill_unspecified"]+
#                                                 x[,"y2015","paper_cardboard.sanitary_landfill_landfill_gas_system"]+
#                                                 x[,"y2015","wood_waste.sanitary_landfill_landfill_gas_system"]+
#                                                 x[,"y2015","paper_cardboard.compost"]+x[,"y2015","wood_waste.compost"]+
#                                                 x[,"y2015","paper_cardboard.recycling"]+x[,"y2015","wood_waste.recycling"]+
#                                                 x[,"y2015","paper_cardboard.incineration"]+x[,"y2015","wood_waste.incineration"]+
#                                                 x[,"y2015","paper_cardboard.dumps"]+x[,"y2015","wood_waste.dumps"]),
#                                 regression=pap_wood_recyc)
#   
#   # pap_wood_compost_calib <- calibration(a, numerator = (x[,"y2015","paper_cardboard.compost"]+x[,"y2015","wood_waste.compost"]),
#   #                                 denominator =(x[,"y2015","paper_cardboard.controlled_landfill"]+x[,"y2015","wood_waste.controlled_landfill"]+
#   #                                                 x[,"y2015","paper_cardboard.landfill_unspecified"]+x[,"y2015","wood_waste.landfill_unspecified"]+
#   #                                                 x[,"y2015","paper_cardboard.sanitary_landfill_landfill_gas_system"]+
#   #                                                 x[,"y2015","wood_waste.sanitary_landfill_landfill_gas_system"]+
#   #                                                 x[,"y2015","paper_cardboard.compost"]+x[,"y2015","wood_waste.compost"]+
#   #                                                 x[,"y2015","paper_cardboard.incineration"]+x[,"y2015","wood_waste.incineration"]+
#   #                                                 x[,"y2015","paper_cardboard.dumps"]+x[,"y2015","wood_waste.dumps"]),
#   #                                 regression=pap_wood_compost)
#   
#   pap_wood_inci_calib <- calibration(a, numerator = (x[,"y2015","paper_cardboard.compost"]+x[,"y2015","wood_waste.compost"]),
#                                         denominator =(x[,"y2015","paper_cardboard.controlled_landfill"]+x[,"y2015","wood_waste.controlled_landfill"]+
#                                                         x[,"y2015","paper_cardboard.landfill_unspecified"]+x[,"y2015","wood_waste.landfill_unspecified"]+
#                                                         x[,"y2015","paper_cardboard.sanitary_landfill_landfill_gas_system"]+
#                                                         x[,"y2015","wood_waste.sanitary_landfill_landfill_gas_system"]+
#                                                         x[,"y2015","paper_cardboard.incineration"]+x[,"y2015","wood_waste.incineration"]+
#                                                         x[,"y2015","paper_cardboard.dumps"]+x[,"y2015","wood_waste.dumps"]),
#                                         regression=pap_wood_inci)
#   #alternative
#   # pap_wood_compost_inci_calib <- calibration(a, numerator = (x[,"y2015","paper_cardboard.compost"]+x[,"y2015","wood_waste.compost"]+
#   #                                                           x[,"y2015","paper_cardboard.incineration"]+x[,"y2015","wood_waste.incineration"]),
#   #                                    denominator =(x[,"y2015","paper_cardboard.controlled_landfill"]+x[,"y2015","wood_waste.controlled_landfill"]+
#   #                                                    x[,"y2015","paper_cardboard.landfill_unspecified"]+x[,"y2015","wood_waste.landfill_unspecified"]+
#   #                                                    x[,"y2015","paper_cardboard.sanitary_landfill_landfill_gas_system"]+
#   #                                                    x[,"y2015","wood_waste.sanitary_landfill_landfill_gas_system"]+
#   #                                                    x[,"y2015","paper_cardboard.incineration"]+x[,"y2015","wood_waste.incineration"]+
#   #                                                    x[,"y2015","paper_cardboard.dumps"]+x[,"y2015","wood_waste.dumps"]),
#   #                                    regression=pap_wood_compost_inci)
#   
#   pap_wood_dumps_calib <- calibration(a, numerator = (x[,"y2015","paper_cardboard.dumps"]+x[,"y2015","wood_waste.dumps"]),
#                                      denominator =(x[,"y2015","paper_cardboard.controlled_landfill"]+x[,"y2015","wood_waste.controlled_landfill"]+
#                                                      x[,"y2015","paper_cardboard.landfill_unspecified"]+x[,"y2015","wood_waste.landfill_unspecified"]+
#                                                      x[,"y2015","paper_cardboard.sanitary_landfill_landfill_gas_system"]+
#                                                      x[,"y2015","wood_waste.sanitary_landfill_landfill_gas_system"]+
#                                                      x[,"y2015","paper_cardboard.dumps"]+x[,"y2015","wood_waste.dumps"]),
#                                      regression=pap_wood_dumps)
#   
#   plas_rubb_other_recyc_calib <- calibration(a, numerator = (x[,"y2015","plastic.recycling"]+x[,"y2015","rubber_leather.recycling"]+x[,"y2015","other.recycling"]),
#                                       denominator =(x[,"y2015","plastic.controlled_landfill"]+x[,"y2015","rubber_leather.controlled_landfill"]+x[,"y2015","other.controlled_landfill"]+
#                                                       x[,"y2015","plastic.landfill_unspecified"]+x[,"y2015","rubber_leather.landfill_unspecified"]+x[,"y2015","other.landfill_unspecified"]+
#                                                       x[,"y2015","plastic.sanitary_landfill_landfill_gas_system"]+
#                                                       x[,"y2015","rubber_leather.sanitary_landfill_landfill_gas_system"]+x[,"y2015","other.sanitary_landfill_landfill_gas_system"]+
#                                                       x[,"y2015","plastic.recycling"]+x[,"y2015","rubber_leather.recycling"]+x[,"y2015","other.recycling"]+
#                                                       x[,"y2015","plastic.incineration"]+x[,"y2015","rubber_leather.incineration"]+x[,"y2015","other.incineration"]+
#                                                       x[,"y2015","plastic.dumps"]+x[,"y2015","rubber_leather.dumps"]+x[,"y2015","other.dumps"]),
#                                       regression=plas_rubb_other_recyc)
#   
#   plas_rubb_other_inci_calib <- calibration(a, numerator = (x[,"y2015","plastic.incineration"]+x[,"y2015","rubber_leather.incineration"]+x[,"y2015","other.incineration"]),
#                                              denominator =(x[,"y2015","plastic.controlled_landfill"]+x[,"y2015","rubber_leather.controlled_landfill"]+x[,"y2015","other.controlled_landfill"]+
#                                                              x[,"y2015","plastic.landfill_unspecified"]+x[,"y2015","rubber_leather.landfill_unspecified"]+x[,"y2015","other.landfill_unspecified"]+
#                                                              x[,"y2015","plastic.sanitary_landfill_landfill_gas_system"]+
#                                                              x[,"y2015","rubber_leather.sanitary_landfill_landfill_gas_system"]+x[,"y2015","other.sanitary_landfill_landfill_gas_system"]+
#                                                              x[,"y2015","plastic.incineration"]+x[,"y2015","rubber_leather.incineration"]+x[,"y2015","other.incineration"]+
#                                                              x[,"y2015","plastic.dumps"]+x[,"y2015","rubber_leather.dumps"]+x[,"y2015","other.dumps"]),
#                                              regression=plas_rubb_other_inci)
#   #alternate better
#   plas_rubb_other_recyc_inci_calib <- calibration(a, numerator = (x[,"y2015","plastic.incineration"]+x[,"y2015","rubber_leather.incineration"]+x[,"y2015","other.incineration"]+
#                                                                  x[,"y2015","plastic.recycling"]+x[,"y2015","rubber_leather.recycling"]+x[,"y2015","other.recycling"] ),
#                                             denominator =(x[,"y2015","plastic.controlled_landfill"]+x[,"y2015","rubber_leather.controlled_landfill"]+x[,"y2015","other.controlled_landfill"]+
#                                                             x[,"y2015","plastic.landfill_unspecified"]+x[,"y2015","rubber_leather.landfill_unspecified"]+x[,"y2015","other.landfill_unspecified"]+
#                                                             x[,"y2015","plastic.sanitary_landfill_landfill_gas_system"]+
#                                                             x[,"y2015","rubber_leather.sanitary_landfill_landfill_gas_system"]+x[,"y2015","other.sanitary_landfill_landfill_gas_system"]+
#                                                             x[,"y2015","plastic.incineration"]+x[,"y2015","rubber_leather.incineration"]+x[,"y2015","other.incineration"]+
#                                                             x[,"y2015","plastic.recycling"]+x[,"y2015","rubber_leather.recycling"]+x[,"y2015","other.recycling"]+
#                                                             x[,"y2015","plastic.dumps"]+x[,"y2015","rubber_leather.dumps"]+x[,"y2015","other.dumps"]),
#                                             regression=plas_rubb_other_recyc_inci)
#   
#   plas_rubb_other_dumps_calib <- calibration(a, numerator = (x[,"y2015","plastic.dumps"]+x[,"y2015","rubber_leather.dumps"]+x[,"y2015","other.dumps"]),
#                                             denominator =(x[,"y2015","plastic.controlled_landfill"]+x[,"y2015","rubber_leather.controlled_landfill"]+x[,"y2015","other.controlled_landfill"]+
#                                                             x[,"y2015","plastic.landfill_unspecified"]+x[,"y2015","rubber_leather.landfill_unspecified"]+x[,"y2015","other.landfill_unspecified"]+
#                                                             x[,"y2015","plastic.sanitary_landfill_landfill_gas_system"]+
#                                                             x[,"y2015","rubber_leather.sanitary_landfill_landfill_gas_system"]+x[,"y2015","other.sanitary_landfill_landfill_gas_system"]+
#                                                             x[,"y2015","plastic.dumps"]+x[,"y2015","rubber_leather.dumps"]+x[,"y2015","other.dumps"]),
#                                             regression=plas_rubb_other_dumps)
#   
# #everything back up to original quantities
# getNames(food_yard_compost_calib) <- paste0("organic.compost.", getNames(food_yard_compost_calib))
# 
# food_yard_inci_calib2 <- food_yard_inci_calib * (1-food_yard_compost_calib)
# food_yard_dumps_calib2 <- food_yard_dumps_calib * (1-(food_yard_compost_calib+food_yard_inci_calib2))
# food_yard_landfills_calib2 <- (1-food_yard_dumps_calib)*(1-(food_yard_compost_calib+food_yard_inci_calib2))
# 
# getNames(food_yard_inci_calib2) <- gsub("compost", "incineration", getNames(food_yard_inci_calib))
# getNames(food_yard_dumps_calib2) <- gsub("compost", "dumps", getNames(food_yard_dumps_calib2))
# getNames(food_yard_landfills_calib2) <- gsub("landfills", "dumps", getNames(food_yard_landfills_calib2))
# 
# organic <- mbind( )
# #mbind 
# 
# #glass metal
# getNames(glassmetal_recyc_calib) <- paste0("glass_metal.recycling.", getNames(glassmetal_recyc_calib))
# 
# glass_metal_dumps_calib2 <- glass_metal_dumps_calib * (1-glassmetal_recyc_calib)
# glass_metal_landfills_calib2 <- (1-glass_metal_dumps_calib)*(1-(food_yard_compost_calib+food_yard_inci_calib2))
# 
# 
# 
# getNames(food_yard_inci_calib2) <- gsub("compost", "incineration", getNames(food_yard_inci_calib))
# getNames(food_yard_dumps_calib2) <- gsub("compost", "dumps", getNames(food_yard_dumps_calib2))
# getNames(food_yard_landfills_calib2) <- gsub("landfills", "dumps", getNames(food_yard_landfills_calib2))
# 
# 
# 
# 
# 
# ##alternate.. doesn't =1 ?
# compost_inci_split <- (x[,,"food.compost"]+x[,,"yard.compost"])/ 
#                       (x[,,"food.compost"]+x[,,"yard.compost"]+x[,,"food.incineration"]+x[,,"yard.incineration"])
# food_yard_compost_calib3 <- food_yard_compost_inci_calib * setYears(compost_inci_split,NULL)
# food_yard_inci_calib3 <- food_yard_compost_inci_calib * setYears((1-compost_inci_split),NULL)
# 
# #mbind
# 
# #split compositiong combinations i.e. foodyard 
#   
#     
# 
# ratio_food_yard <- x[,,"food"]/(x[,,"food"]+x[,,"yard"])
# 
# ratio_paper_wood <- x[,,"paper_cardboard"]/(x[,,"paper_cardboard"]+x[,,"wood_waste"])
# ratio_plastic_rubber_other <- x[,,"plastic"]/(x[,,"plastic"]+x[,,"rubber_leather"]+x[,,"other"])
# ratio_other_plastic_rubber <- x[,,"other"]/(x[,,"plastic"]+x[,,"rubber_leather"]+x[,,"other"])
# ratio_controlled_landfill <- x[,,"controlled_landfill"]/(x[,,"landfill_unspecified"]+x[,,"controlled_landfill"]+x[,,"sanitary_landfill_landfill_gas_system"])
# ratio_landfill_unspecified <- x[,,"landfill_unspecified"]/(x[,,"landfill_unspecified"]+x[,,"controlled_landfill"]+x[,,"sanitary_landfill_landfill_gas_system"])
# 
# 
# 
# 
# 
# 
# 
# #####
# 
#   ratio_org<- x[,,"organic.compost"]/(x[,,"organic.wte"]+x[,,"organic.compost"])
#   #weighted average?? 
#   ratio1 <- ratio_org[which(is.finite(ratio_org[,,"organic.compost.wte"]))]
#   avg<- mean(ratio1[,,"organic.compost.wte"])
#   ratio_org[which(!is.finite(ratio_org[,,"organic.compost.wte"]))] <- avg
#   ratio_wte = 1-ratio_org
#   
#   org_compost <- org_compost_and_wte_calib * setYears(ratio_org,NULL)
#   org_compost <- collapseNames(org_compost, collapsedim=4)
#   
#   org_wte <- org_compost_and_wte_calib * setYears(ratio_wte, NULL)
#   org_wte <- collapseNames(org_wte, collapsedim=3)
#   
#   org_dumps <- org_dumps_over_landfills_calib * (1-(org_compost + org_wte))
#   getNames(org_dumps) <-gsub("compost", "dumps", getNames(org_dumps))
#   org_dumps <- collapseNames(org_dumps, collapsedim=4)
#   
#   org_landfills <- 1-(org_dumps+org_wte+org_compost)
#   getNames(org_landfills) <-gsub("compost", "landfills", getNames(org_landfills))
#   org_landfills <- collapseNames(org_landfills, collapsedim=c(3,4))
#   
#   organic <- mbind(org_compost, org_dumps, org_landfills, org_wte)
#   
#   getNames(glassmetal_recycled_calib, dim=1) <- paste0("glass_metal.recycled.", getNames(glassmetal_recycled_calib))
#   glass_metal_recycled <- glassmetal_recycled_calib
#   glass_metal_recycled<- dimOrder(glass_metal_recycled, c(3,1,2))
#   
#   glass_metal_dumps <- glassmetal_dumps_over_landfills_calib * (1-(glass_metal_recycled))
#   getNames(glass_metal_dumps) <-gsub("recycled", "dumps", getNames(glass_metal_dumps))
#   
#   glass_metal_landfills <- 1- (glass_metal_dumps + glass_metal_recycled)
#   getNames(glass_metal_landfills) <-gsub("recycled", "landfills", getNames(glass_metal_landfills))
#   glass_metal_landfills <- collapseNames(glass_metal_landfills, collapsedim=3)
#   
#   glass_metal <- mbind(glass_metal_recycled, glass_metal_landfills, glass_metal_dumps)
#   
#   ratio_pap<- x[,,"paper.recycled"]/(x[,,"paper.wte"]+x[,,"paper.recycled"])
#   
#   #weighted average?? right now every country gets historical average... 
#   ratio2 <- ratio_pap[which(is.finite(ratio_pap[,,"paper.recycled.wte"]))]
#   avg2<- mean(ratio2[,,"paper.recycled.wte"])
#   ratio_pap[which(!is.finite(ratio_pap[,,"paper.recycled.wte"]))] <- avg2
#   ratio_wte = 1-ratio_pap
#   
#   pap_recycled <- paper_recycled_and_wte_calib * setYears(ratio_pap,NULL)
#   pap_recycled <- collapseNames(pap_recycled, collapsedim=4)
#   
#   pap_wte <- paper_recycled_and_wte_calib * setYears(ratio_wte, NULL)
#   pap_wte <- collapseNames(pap_wte, collapsedim=3)
#   
#   pap_dumps <- paper_dumps_over_landfills_calib * (1-(pap_recycled + pap_wte))
#   getNames(pap_dumps) <-gsub("recycled", "dumps", getNames(pap_dumps))
#   pap_dumps <- collapseNames(pap_dumps, collapsedim=4)
#   
#   pap_landfills <- 1-(pap_dumps+pap_wte+pap_recycled)
#   getNames(pap_landfills) <-gsub("recycled", "landfills", getNames(pap_landfills))
#   pap_landfills <- collapseNames(pap_landfills, collapsedim=c(3,4))
#   
#   paper <- mbind(pap_recycled, pap_dumps, pap_landfills, pap_wte)
#   
#   ratio_plast<- x[,,"plastic.recycled"]/(x[,,"plastic.wte"]+x[,,"plastic.recycled"])
#   
#   #weighted average?? right now every country gets historical average... 
#   ratio3 <- ratio_plast[which(is.finite(ratio_plast[,,"plastic.recycled.wte"]))]
#   avg2<- mean(ratio3[,,"plastic.recycled.wte"])
#   ratio_plast[which(!is.finite(ratio_plast[,,"plastic.recycled.wte"]))] <- avg2
#   ratio_wte = 1-ratio_plast
#   
#   plast_recycled <- plastic_recycled_and_wte_calib * setYears(ratio_plast,NULL)
#   plast_recycled <- collapseNames(plast_recycled, collapsedim=4)
#   
#   plast_wte <- plastic_recycled_and_wte_calib * setYears(ratio_wte, NULL)
#   plast_wte <- collapseNames(plast_wte, collapsedim=3)
#   
#   plast_dumps <- plastic_dumps_over_landfills_calib * (1-(plast_recycled + plast_wte))
#   getNames(plast_dumps) <-gsub("recycled", "dumps", getNames(plast_dumps))
#   plast_dumps <- collapseNames(plast_dumps, collapsedim=4)
#   
#   plast_landfills <- 1-(plast_dumps+plast_wte+plast_recycled)
#   getNames(plast_landfills) <-gsub("recycled", "landfills", getNames(plast_landfills))
#   plast_landfills <- collapseNames(plast_landfills, collapsedim=c(3,4))
#   
#   plastic <- mbind(plast_recycled, plast_dumps, plast_landfills, plast_wte)
#   
#   
#   waste <-mbind(organic, paper, glass_metal, plastic)
#   
#   if (share == FALSE)
#     a <- readSource("Waste", subtype="Generation", convert=F)
#   
#   a <- toolCountryFill(a, fill=NA)
#   
#   
#   calibration_factor = (a[,"y2005","generation_pc"]/generation_pc[,"y2005",])
#   calibration_factor[is.na(a)] = 1
#   calibrated = generation_pc_reg * setYears(calibration_factor,NULL)
#   
#   generation_pc <- collapseNames(calibrated, collapsedim=1)
#   
#   waste_pc <- generation_pc * waste
#   
# }
# 
# 
# 
# 
# #for quantities and not percentages
# # separately generate total waste generation  thru separate  regression
# 
# 
# #second scale to calcWasteDistrib but not really possible because data doesn't exist for every country..
# 
# 
# 
# # additive calibration
# #calibration_factor = x[,"y2005",]-generation_regression[,"y2005",]
# #calibration_factor[is.na(x)] = 0
# #calibrated = generation_regression + setYears(calibration_factor,NULL)
# #calibrated[calibrated<0] = 0
# 
# # multiplicative growth rate calibration
# #  calibration_factor = (x[,"y2005","organic.wte"]+x[,"y2005","organic.compost"])/org_compost_and_wte[,"y2005",]
#  calibration_factor[is.na(x)] = 1
#  calibrated = generation_regression * setYears(calibration_factor,NULL)
#  calibrated[calibrated<0] = 1
# x <- collapseNames(calibrated)
# 
#  #take out SRES scenarios for being able to multiply with urban pop later
#  }

#for each subtype of the other sheets

#else if (subtype == "Composition"){
# x<- readSource("Waste", subtype="Composition", convert=F)

#weird things going on with SRES scenarios

#  generation <- readSource(type="Waste", subtype="Generation", convert=F)
# commonregions=intersect(getRegions(generation),getRegions(x))
#  x=x[c(commonregions),,]*generation[c(commonregions),,]/100
# x <- x[,,"generation_pc"]
#    x<- collapseNames(x)
#    
#    comp_pc <- toolCountryFill(x, fill=NA)
#    
#    
# gdp_pc <- calcOutput("GDPpc", aggregate=FALSE, naming="indicator.scenario")
# 
# organic_regression = ((0.2601*gdp_pc)/(2.099e+03+gdp_pc))
#      #multiplicative calib?
#     org_calibration_factor = comp_pc[,"y2005","organic"]/organic_regression[,"y2005",]
#     org_calibration_factor[is.na(comp_pc[,,"organic"])] = 1
#     org_calibrated = organic_regression * setYears(org_calibration_factor,NULL)
#     org_calibrated[org_calibrated<0] = 1
#   org_calibrated <- collapseNames(org_calibrated)
#       getNames(org_calibrated) <- paste0("Organic.", getNames(org_calibrated))
# 
#    #additive calib
#calibration_factor = comp_pc[,"y2005","Organic"]-organic_regression[,"y2005",]
#org_calibration_factor[is.na(comp_pc[,,"Organic"])] = 0
#org_calibrated = organic_regression + setYears(org_calibration_factor,NULL)
#org_calibrated[org_calibrated<0] = 0
#org_calibrated <- collapseNames(org_calibrated)

#paper_regression = (7.572e-06*(gdp_pc)-4.56e-03)
#additive calibration for linear eq
# pap_calibration_factor = comp_pc[,"y2005","paper"]-paper_regression[,"y2005",]
# pap_calibration_factor[is.na(comp_pc[,,"paper"])] = 0
#pap_calibrated = paper_regression + setYears(pap_calibration_factor,NULL)
#pap_calibrated[pap_calibrated<0] = 0
#pap_calibrated <- collapseNames(pap_calibrated)
# getNames(pap_calibrated) <- paste0("Paper.", getNames(pap_calibrated))


#paper_regression = collapseNames(paper_regression)
#quite different values between some reg values and 2005 values  

#x <- mbind(org_calibrated, pap_calibrated)


## calculate totals here for now

# org_calibrated <- org_calibrated[,,c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")]

# pop <- calcOutput("Population", aggregate = F,naming="indicator.scenario")
# urb_shr <- calcOutput("Urban", aggregate=F, naming="indicator.scenario")
#total_urb <- pop*urb_shr

#total_org_gen <- org_calibrated*total_urb
#total_org_gen <- collapseNames(total_org_gen)
#getNames(total_org_gen) <- c("org_gen_SSP1", "org_gen_SSP2", "org_gen_SSP3", "org_gen_SSP4", "gen_SSP5")


# }


#Disposal
#else  if (subtype == "Disposal"){

# x <- toolCountryFill(x, fill=NA)
#disposal less good looking regressions...}

#}
#return(list(x=x))

#}
