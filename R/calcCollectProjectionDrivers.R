#' @importFrom magclass getNames<- place_x_in_y


calcCollectProjectionDrivers<-function(driver="all"){
  
  if(driver!="all"){
    all<-calcOutput("CollectProjectionDrivers",aggregate = FALSE)
    combined<-collapseNames(all[,,driver])
  } else {

    ## add education indicators
    
    pop_ssp_sres<-calcOutput("Population", aggregate = FALSE,naming="indicator.scenario")
    gdp_ssp_sres<-calcOutput("GDPppp", aggregate = FALSE,naming="indicator.scenario")
    gdp_ssp_sres_mer<-calcOutput("GDPppp", GDPpppPast = "IHME_USD05_MER_pc_completed", GDPpppFuture="SRES_SSP_completed", GDPpppCalib="past",aggregate = FALSE,naming="indicator.scenario")
    
    getNames(gdp_ssp_sres_mer,dim = 1)<-"gdpmer"
    
    urban_shr_ssp <- calcOutput("Urban", UrbanCalib="past", UrbanPast="WDI", UrbanFuture="SSP",aggregate=FALSE,naming="indicator.scenario") 
    urban_ssp <- urban_shr_ssp*pop_ssp_sres[,,getNames(urban_shr_ssp)]
    getNames(urban_ssp,dim = 1)<-"urban"
    
    # Demographics
    #Lutz<-calcOutput("Demography",education=FALSE,aggregate=FALSE)  #No division into education groups (due to eductaion=FALSE)
    #population <- dimSums(Lutz, dim=c("age","sex"))
    #population<-add_dimension(x = population,dim = 3.1,add="indicator",nm = "population")
    
    combined<-mbind(
      pop_ssp_sres,
      gdp_ssp_sres,
      gdp_ssp_sres_mer,
      urban_ssp
      #population
    )

  }

  
  
  if (driver=="gdp"){
    unit="Mio USD 05"  
  } else if(driver=="pop") {
    unit="Mio people"
  } else if(driver=="urban") {
    unit="Mio people"
  } else {
    unit="population: Mio people, gdp: Million USD, urban population: Mio people"
  }
  
  return(list(x=combined,
              weight=NULL,
              unit=unit,
              description="collects all data necessary to create demand projections and brings them into a joint format"
  ))
}