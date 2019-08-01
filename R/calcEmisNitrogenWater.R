#' @title calcEmisNitrogenWater
#' @description Calculates Nitrogen Budgets for surface Water on country levels.
#'
#' @param method method for calculating no3_n in groundwater
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EmisNitrogenWater")
#' }
#' @importFrom magclass setNames



calcEmisNitrogenWater<-function(method="Nsurplus"){
  
  if (method=="Nsurplus"){
    method="IPCC"
  } else if (method=="Nsurplus2"){
    method="Nsurplus"
  } else {stop("unknown method")}
  
  groundwater<-dimSums(calcOutput("EmisNitrogenPast",method=method,aggregate = FALSE)[,,"no3_n"],dim=3)
  sewage<-collapseNames(calcOutput("NutrientBudgetSewage",aggregate=FALSE)[,,"nr"][,,"freshwater"])
  
  emis<-setNames(groundwater,"groundwater")
  emis<-add_columns(emis,addnm = c("riparian","freshwater"))
  emis<-add_dimension(emis,dim = 3.2,nm = "n2_n")
  emis<-add_columns(emis,dim = 3.2,addnm = c("n2o_n_direct","accumulation","no3_n","no2_n","nh3_n"))
  emis[,,]<-0
  
  emis[,,"groundwater"][,,"n2_n"]<-groundwater/(93+11)*23  #(93+11): runoff and groundwater
  emis[,,"groundwater"][,,"n2o_n_direct"]<-groundwater/(93+11)*0.2
  emis[,,"groundwater"][,,"accumulation"]<-groundwater/(93+11)*9
  
  emis[,,"riparian"][,,"n2_n"]<-groundwater/(93+11)*5
  emis[,,"riparian"][,,"n2o_n_direct"]<-groundwater/(93+11)*0.9
  
  river_inflow<-groundwater-dimSums(emis,dim=3)+sewage
  
  # only DIN. in Seitzinger 2010: 18.9 in the year 2000 Seitzinger, S. P. et al. Global river nutrient export: A scenario analysis of past and future trends. Global Biogeochem. Cycles 24, GB0A08 (2010).
  # Kroeze et al (2005) then assume 5% of DIN to be lost as N2O in rivers, and 1% in estuaries
  # Add to Bouwmans soil N the sewage point sources that enter surface waters, which are 7.7 Tg
  # 65 TG = soilN--> surface water (Bouwman 2013)
  # 7.7 Tg: 1.Morée, A. L., Beusen, A. H. W., Bouwman, A. F. & Willems, W. J. Exploring global nitrogen and phosphorus flows in urban wastes during the twentieth century. Global Biogeochemical Cycles 27, 836–846 (2013).
  # 43.2 Tg: reaching ocean according to Seitinger
  
  emis[,,"freshwater"][,,"n2o_n_direct"]<-river_inflow / (65+7.7)*(18.9*0.06)
  emis[,,"freshwater"][,,"n2_n"]<- river_inflow /(65+7.7)*(65+7.7-43.2-dimSums(emis[,,"freshwater"][,,"n2o_n_direct"],dim=c(1,3)))
  
  #dimSums(outputs,dim=c(1,3))/dimSums(inputs,dim=c(1,3))
  return(list(
    x=emis,
    weight=NULL,
    unit="Mt Nr",
    description="Nitrogen emissions from surface waters"))
}
