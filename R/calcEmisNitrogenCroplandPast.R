#' @title calcEmisNitrogenCroplandPast
#' @description calculates nitrogenous emissions from croplands in the historical period.
#'
#' @param method IPCC: emissions are calculated according the the IPCC 2006 National Guidelines for Greenhouse Gas Inventories. Nsurplus: Emissions in 2005 are calculated according to IPCC, and the scaled with nitrogen losses from croplands.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EmisNitrogenCroplandPast")
#' }
#' 


calcEmisNitrogenCroplandPast<-function(method="IPCC"){
  
  if (method=="IPCC"){
    # deposition has to be false in budgets, otherwhise endless loop
    budget<-calcOutput("NitrogenBudgetCropland",aggregate=FALSE,deposition="CEDS")
    
    fert_rice<-collapseNames(calcOutput("FertilizerByCrop",deposition="CEDS",aggregate = FALSE)[,,"fertilizer"][,,"rice_pro"])
    ef<-setYears(readSource("IPCC","efnsoil",convert=FALSE),NULL)
    
    #replace leaching emission factor by more precise estimate
    fracLeach<-collapseNames(calcOutput("IPCCfracLeach",aggregate = FALSE,cellular=FALSE)[,,"crop"])
    tmp<-fracLeach*ef
    tmp[,,]<-ef
    tmp[,,"no3_n"]<-fracLeach
    tmp[,,"no3_n"][,,"rice"]<-0
    ef<-tmp
    
    out<-new.magpie(getRegions(budget),getYears(budget),getNames(ef))
    out[,,"inorg_fert"]=collapseNames(budget[,,"fertilizer"])*ef[,,"inorg_fert"]
    out[,,"man_crop"]=collapseNames(budget[,,"manure"])*ef[,,"man_crop"]
    out[,,"resid"]=dimSums(budget[,,c("ag_recycling","bg_recycling")],dim=3.1)*ef[,,"resid"]
    out[,,"som"]=collapseNames(budget[,,"som"])*ef[,,"som"]
    out[,,"rice"]=fert_rice*ef[,,"rice"]
    
    out=dimOrder(out,perm=c(2,1))
    
    
  } else if (method %in% c("Nsurplus","Nsurplus2")){
    
    # IPCC methodologies are not in line with the Nr surplus in various regions. Sometimes, volatilization and leaching exceed the surplus.
    # Also, a change in NUE does not change the emission factors.
    # Emission factors in IPCC were estimated for average management conditions in 2006 and rather for global estimates.
    # Finally, the emissions only include fertilization-induced emissions, not total emissions that include also the natural emissions
    #
    # In order to reach emission factors that scale with the Nr surplus, we used the following steps:
    #   - We estimate NH3 NOx NO3 and N2O emissions based on the IPCC tier 1 methodology on country level.
    #   - We add non-anthropogenic emissions as they are not included in IPCC
    #   - We add indirect emissions from atmospheric deposition, which are attribtued to the source in IPCC
    #   - We estimate globally for the year 2005 the share of denitrification as 1-(NOx,NH3,)
    #   - We calculate N2 emissions applying these global shares for N2O and N2 to the national N surplus
    #   - We rescale all emission types by one factor for each country such that the sum of all emissions is equal to the Nr surplus.
    #   - We estimate globally for the year 2005 the share of direct N2O emission in denitrification using IPCC tier 1 methodology to subdivide denitrification into N2O and N2
    
    baseyear="y2005"
    
    
    
    emis <- calcOutput("EmisNitrogenCroplandPast",method="IPCC",aggregate = FALSE)

    # first iteration: calculate atmospheric deposition based on CEDS and estimate leaching
    # second iteration: calculate deposition based on Nsurplus and Oceans based on leaching    
    if(method=="Nsurplus2"){
      dep <- calcOutput("AtmosphericDeposition",aggregate=FALSE,cellular=FALSE,datasource="Nsurplus")
      budget<-calcOutput("NitrogenBudgetCropland",aggregate=FALSE,deposition="Nsurplus")
      method="Nsurplus"
    } else {
      dep <- calcOutput("AtmosphericDeposition",aggregate=FALSE,cellular=FALSE,datasource="CEDS")
      budget<-calcOutput("NitrogenBudgetCropland",aggregate=FALSE,deposition="CEDS")
    }
    
    
    # Add indirect deposition emissions for N2O ####
    ef<-setYears(readSource("IPCC","emissionfactors",convert=FALSE),NULL)
    emis_dep <- dimSums(
      dep[,,"crop"]
      ,dim=3) * ef[,,"ef_5"]
    emis<-add_columns(emis,addnm = "deposition",dim = 3.1)
    emis[,,"deposition"]<-0
    emis[,,"n2o_n_direct"][,,"deposition"] <- emis_dep
    
    # Add natural emissions ####
    emis_natural<-collapseNames(calcOutput("EmisNitrogenPreagriculture",aggregate=FALSE,deposition=FALSE)[,,"crop"][,,c("n2_n","accumulation"),invert=TRUE])
    emis<-add_columns(emis,addnm = "natural",dim = 3.1)
    emis[,,"natural"]<-0
    emis[,,"natural"] <- emis_natural
    

    # add dinitrification ####
    
    emis_sum <- dimSums(emis,dim=c(3.1))
    
    n2<-setNames(budget[,,"surplus"]-dimSums(emis_sum,dim=3),"n2_n")
    n2[n2<0]<-0
    
    emis_sum<-add_columns(emis_sum,addnm = c("n2_n"),dim=3.1)
    
    emis_sum[,,"n2_n"]<-n2
    
    # Scaling emissions with surplus ####
    

    
    denitrification_shr = setNames(dimSums(emis_sum[,,c("n2o_n_direct","n2_n")],dim=c(1,3))/dimSums(budget[,,"surplus"],dim=c(1,3)),NULL)
    
    unscaled<-mbind(emis_sum[,,c("nh3_n","no2_n","no3_n")],
                    setNames(denitrification_shr*budget[,,"surplus"],"denitrification"))
    
    emission_shares<-setYears(unscaled[,baseyear,]/dimSums(unscaled[,baseyear,],dim=3),NULL)
    emission_shares_glo<-setYears(dimSums(unscaled[,baseyear,],dim=1,na.rm=TRUE)/dimSums(unscaled[,baseyear,],dim=c(1,3),na.rm=TRUE),NULL)
    emission_shares[which(dimSums(emis_sum[,,],dim=c(2,3))<0.001),,]=emission_shares_glo

    #distributing cropland surplus according to these shares.
    emissions<-collapseNames(budget[,,"surplus"])*emission_shares
    
    # assume globally same fraction of N2O in dentrification process. For comparison: Bessou et al comes to approximately 11%.
    # Bessou, C., B. Mary, J. Léonard, M. Roussel, E. Gréhan, and B. Gabrielle. 2010. “Modelling Soil Compaction Impacts on Nitrous Oxide Emissions in Arable Fields.” European Journal of Soil Science 61 (3): 348–63. doi:10.1111/j.1365-2389.2010.01243.x.
    
    vcat(1,"n2o emissions also occur in denitrication process. Nsurplus method should consider this")
    
    emissions<-add_columns(x=emissions,addnm=c("n2o_n_direct","n2_n"),dim = 3.1)
    emis_n2on_share=sum(emis_sum[,baseyear,c("n2o_n_direct")])/sum(emissions[,baseyear,c("denitrification")])
    emissions[,,"n2o_n_direct"]=dimSums(emissions[,,c("denitrification")],dim=3.1)*emis_n2on_share
    emissions[,,"n2_n"]=dimSums(emissions[,,c("denitrification")],dim=3.1)*(1-emis_n2on_share)
    
    out<-emissions[,,"denitrification",invert=TRUE]
    out<-add_dimension(out,nm = "cropland_soils",dim = 3.1)
  }
  

  return(list(
    x=out,
    weight=NULL,
    unit="Mt Nr in various forms",
    description="Nitrogen losses from cropland soils"))
}