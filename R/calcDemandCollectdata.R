#' @importFrom magclass getNames<- place_x_in_y


calcDemandCollectdata<-function(){
  
  massbalance <- calcOutput("FAOmassbalance",aggregate = F)
  indicators  <- c("kcal_fao","livst_kcal_fao","fish_kcal_fao","vegfruit_kcal_fao",
                   "material",
                   "pop_hist",
                   "pop_SSP1","pop_SSP2","pop_SSP3","pop_SSP4","pop_SSP5",
                   "pop_a1","pop_a2","pop_b1","pop_b2",
                   "gdp_hist",
                   "gdp_SSP1","gdp_SSP2","gdp_SSP3","gdp_SSP4","gdp_SSP5",
                   "gdp_a1","gdp_a2","gdp_b1","gdp_b2",
                   "urban_hist",
                   "urban_SSP1","urban_SSP2","urban_SSP3","urban_SSP4","urban_SSP5")
  
  
  fullobject   <- new.magpie(getRegions(massbalance), findset("time"), indicators)
  weightobject <- fullobject
  weightobject[,,] <- NA
  
  pop_history       <- setNames(calcOutput("PopulationPast", PopulationPast="WDI_completed", aggregate = FALSE),"pop_hist")
  pop_ssp_sres      <- calcOutput("Population",PopulationCalib = "past", PopulationPast="WDI", PopulationFuture="SRES_SSP_completed", aggregate = FALSE)
 
  gdp_history       <- setNames(calcOutput("GDPpppPast",GDPpppPast = "IHME_USD05_MER_pc_completed", aggregate=FALSE),"gdp_hist")
  gdp_ssp_sres      <- calcOutput("GDPppp",GDPpppCalib = "past",GDPpppPast = "IHME_USD05_MER_pc",GDPpppFuture = "SRES_SSP_completed", aggregate = FALSE)

  
  urban_shr_history <- setNames(calcOutput("UrbanPast", UrbanPast="WDI", aggregate=FALSE) ,"urban_hist")
  urban_shr_ssp     <- calcOutput("Urban", UrbanCalib="past", UrbanPast="WDI", UrbanFuture="SSP",aggregate=FALSE) 
  urban_history     <- urban_shr_history*setNames(pop_history,NULL)
  urban_ssp         <- urban_shr_ssp*pop_ssp_sres[,,getNames(urban_shr_ssp)]
  getNames(urban_ssp) <- paste0("urban_",substring(getNames(urban_ssp),first = 5,last = 8))
  
#   fullobject_history<-mbind(pop_history,
#                             gdp_history,
#                             urban_history)
#   fullobject_ssp<-mbind(pop_ssp_sres,
#                         gdp_ssp_sres,
#                         urban_ssp)
  
  fullobject <- place_x_in_y(x=pop_history[,,intersect(getNames(pop_history), getNames(fullobject))],     y=fullobject,expand = F)
  fullobject <- place_x_in_y(x=pop_ssp_sres[,,intersect(getNames(pop_ssp_sres), getNames(fullobject))],   y=fullobject,expand = F)
  fullobject <- place_x_in_y(x=gdp_history[,,intersect(getNames(gdp_history), getNames(fullobject))],     y=fullobject,expand = F)
  fullobject <- place_x_in_y(x=gdp_ssp_sres[,,intersect(getNames(gdp_ssp_sres), getNames(fullobject))],   y=fullobject,expand = F)
  fullobject <- place_x_in_y(x=urban_history[,,intersect(getNames(urban_history), getNames(fullobject))], y=fullobject,expand = F)
  fullobject <- place_x_in_y(x=urban_ssp[,,intersect(getNames(urban_ssp), getNames(fullobject))],         y=fullobject,expand = F)
  
  Names_diff <- setdiff(c(getNames(pop_history),
                          getNames(pop_ssp_sres), 
                          getNames(gdp_history),
                          getNames(gdp_ssp_sres), 
                          getNames(urban_history),
                          getNames(urban_ssp)),
                        getNames(fullobject))
  if(length(Names_diff) != 0) vcat(verbosity = 2 , paste("Data for", Names_diff,"scenario will be ignored, 
                                                         since not all data is provided for that scenarios"))
  
  
  # food demand
  
  kcal          <- massbalance[,,"households"][,,"ge"]
  vcat(verbosity = 2 ,paste("South Sudan (SSD) seems to have a data error. Kcal set to 0"))
  kcal["SSD",,] <- NaN
  kcal          <- dimSums(kcal/365/4.184,dim=c(3.2,3.3))*1000000
  kcal[which(kcal==Inf)] < -NaN
  
  kcal_fao          <- setNames(dimSums(kcal[,,],dim=3.1),"kcal_fao")
  livst_kcal_fao    <- setNames(dimSums(kcal[,,findset("kap")],dim=3.1),"livst_kcal_fao")
  fish_kcal_fao     <- setNames(dimSums(kcal[,,"fish"],dim=3.1),"fish_kcal_fao")
  vegfruit_kcal_fao <- setNames(dimSums(kcal[,,"others"],dim=3.1),"vegfruit_kcal_fao")

  material <- setNames(dimSums(massbalance[,,"other_util"][,,"dm"],dim=c(3.1,3.2,3.3)),"material")
  
#   fullobject_kcal<-mbind(kcal_fao,
#                          livst_kcal_fao,
#                          fish_kcal_fao,
#                          vegfruit_kcal_fao,
#                          material)
#   
  fullobject <- place_x_in_y(x=kcal_fao,          y=fullobject,expand = F)  
  fullobject <- place_x_in_y(x=livst_kcal_fao,    y=fullobject,expand = F) 
  fullobject <- place_x_in_y(x=fish_kcal_fao,     y=fullobject,expand = F) 
  fullobject <- place_x_in_y(x=vegfruit_kcal_fao, y=fullobject,expand = F)   
  fullobject <- place_x_in_y(x=material,          y=fullobject,expand = F)  
  
  fullobject[is.na(fullobject)] <- 0
  vcat(verbosity = 2,"NAs were replaced with 0s")
  
  return(list(x=fullobject,
              weight=NULL,
              unit="food supply: kcal, material:Mt DM, population: Mio people, gdp: Million USD, urban population: Mio people",
              description="collects all data necessary to creat demand projections and brings them into a joint format"
              ))
}