#' @title calcEF3confinement
#' @description Emission factors for nitrogenous emissions in livestock confinements
#'
#' @param products Either livestock products in MAgPIE or IPCC products
#' @param selection defaults to n_pollutants_direct
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EF3confinement")
#' }
#' @importFrom magclass getYears<-
#' @importFrom madrat toolNAreplace


calcEF3confinement<-function(products="magpie",selection="n_pollutants_direct"){

  FracGas<-readSource("IPCC",subtype="fracgasms",convert=FALSE)
  FracN2O<-FracGas
  FracN2O[,,]<-NA
  FracN2O[,,]<-readSource("IPCC",subtype="awmsconfef3",convert=FALSE)
  FracLoss<-readSource("IPCC",subtype="fraclossms",convert=FALSE)
  if(any(FracLoss-FracGas-FracN2O<0)){
    vcat(verbosity =1,"AWMS: FracGas, FracN2O and FracLoss are incompatible. Increasing FracLossms")
    FracLoss<-FracLoss+FracN2O
  }
  # assumption: half of remaining losses are from leaching, which matches tier-2 estimates of 1-20% of leaching
  FracN2<-FracNO3N<-(FracLoss-FracGas-FracN2O)/2
  no2_share_volat_awms<-collapseNames(readSource("IPCC","emissionfactors",convert = FALSE)[,,"no2_share_volat_awms"])
  FracNO2<-FracGas*no2_share_volat_awms
  FracNH3<-FracGas*(1-no2_share_volat_awms)
  distribution<-mbind(
    add_dimension(FracN2O,nm = "n2o_n_direct",dim = 3.1),
    add_dimension(FracNH3,nm = "nh3_n",dim = 3.1),
    add_dimension(FracNO2,nm = "no2_n",dim = 3.1),
    add_dimension(FracNO3N,nm = "no3_n",dim = 3.1),
    add_dimension(FracN2,nm = "n2_n",dim = 3.1),
    add_dimension(1-(FracN2O+FracNH3+FracNO2+FracNO3N+FracN2),nm = "recycling",dim = 3.1)
  )
  awms<-getNames(distribution,dim=3)
  excretion<-setYears(calcOutput("ExcretionIPCC",products="IPCC",aggregate = FALSE)[,"y2005",awms],NULL)
  if(products=="magpie") {
    #add a marginal amount to each animal waste management system to avoid NAs
    excretion<-excretion+0.00000001
    tmp=excretion * distribution
    map<-toolMappingFile(type = "sectoral",name="IPCCitems.csv",readcsv=T)
    tmp=toolAggregate(x = tmp,dim = 3.1,rel = map,from = "ipcc",to="magpie",partrel = T)
    excretion=toolAggregate(x = excretion,dim = 3.1,rel = map,from = "ipcc",to="magpie",partrel = T)
    distribution<-tmp<-tmp/excretion
    tmp[,,]<-1
    excretion<-excretion*tmp
  }  else if (products=="IPCC"){
    excretion<-excretion+0.00000001
    tmp<-excretion
    tmp[,,]<-1
    tmp<-distribution<-distribution*tmp
    tmp[,,]<-1
    excretion<-tmp*excretion
  }
  
  getYears(distribution)<-NULL
  getYears(excretion)<-NULL
  
  if(!is.null(selection)){
    if(selection=="n_pollutants_direct"){
      selection<-findset("n_pollutants_direct")
    }
    distribution=collapseNames(distribution[,,selection])
    excretion=collapseNames(excretion[,,selection])
  } 
  
  out<-toolNAreplace(x = distribution,weight=excretion,replaceby = 0)
  
  return(list(x=out$x,
              weight=out$weight,
              unit="share",
              description="share of nitrogen in managed manure emitted in various forms or recycled")
  ) 
}
