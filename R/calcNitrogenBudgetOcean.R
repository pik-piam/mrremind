#' @title calcNitrogenBudgetOcean
#' @description Calculates Nitrogen Budgets for Oceans on global level. Values are placed in Antarcica (ATA)
#' @param deposition Method for calculating Atmospheric deposition: Nsurplus2 and Nsurplus are based on deposition rates based on own emission calculations after 2 or after 1 iteration, respectively.
#' @param leaching Method for calculating leaching: Nsurplus2 and Nsurplus are based on deposition rates based on own emission calculations after 2 or after 1 iteration, respectively.
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NitrogenBudgetOcean")
#' }
#' @importFrom magclass setNames


calcNitrogenBudgetOcean<-function(deposition="ACCMIP",leaching="Nsurplus"){
  vcat(2,"strange to change leaching attribute in if statement. Is this correct?")
  past<-findset("past")
  dep<-calcOutput("AtmosphericDeposition",datasource=deposition,glo_incl_oceans=FALSE,cellular=FALSE,emission=FALSE,aggregate = FALSE)
  dep_glo<-calcOutput("AtmosphericDeposition",datasource=deposition,glo_incl_oceans=TRUE,cellular=FALSE,emission=FALSE,aggregate = FALSE)
  
  deposition=(dimSums(dep_glo,dim=3)-dimSums(dep,dim=c(1,3)))[,past,]
  
  if(leaching=="Nsurplus"){
    groundwater<-dimSums(calcOutput("EmisNitrogenPast",method="IPCC",aggregate = FALSE)[,,"no3_n"],dim=c(3.1,3.2))  
  } else if (leaching=="Nsurplus2"){
    groundwater<-dimSums(calcOutput("EmisNitrogenPast",method="Nsurplus",aggregate = FALSE)[,,"no3_n"],dim=c(3.1,3.2))  
  }
  
  sewage<-collapseNames(calcOutput("NutrientBudgetSewage",aggregate = FALSE)[,,"nr"][,,"freshwater"])
  
  waterloss<-calcOutput("EmisNitrogenWater",method=leaching,aggregate = FALSE)
  fish<-collapseNames(calcOutput("FAOmassbalance",aggregate = FALSE)[,,"fish"][,,"production"][,,"nr"])
  
  riverdischarge <- setNames(groundwater+sewage-dimSums(waterloss,dim=3),"riverdischarge")
  
  budget<-add_columns(riverdischarge,addnm = c("fixation_ocean","deposition","fish","surplus"))
  budget[,,c("fixation_ocean","deposition","fish","surplus")]=0
  budget[,,"fish"]<-fish
  # 140 Tg: Voss, M. et al. The marine nitrogen cycle: recent discoveries, uncertainties and the potential relevance of climate change. Philosophical Transactions of the Royal Society B: Biological Sciences 368, 20130121–20130121 (2013).
  budget["ATA",,"fixation_ocean"]<-140
  budget["ATA",,"deposition"]<-deposition
  budget["ATA",,"surplus"]<-dimSums(budget[,,c("fixation_ocean","deposition")],dim=c(1,3))-dimSums(budget[,,c("fish")],dim=c(1,3))
  
  vcat(2,"Fish production is allocated to oceans, but happens in both Oceans and Inland water bodies")
  
  return(list(
    x=budget,
    weight=NULL,
    unit="Mt Nr",
    description="Nitrogen budget for oceans"))
}
