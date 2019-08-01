#' @title calcPhysicalInactivity
#' @description physical inactivity level estimate based on 
#' Hallal, Pedro C, Lars Bo Andersen, Fiona C Bull, Regina Guthold, William Haskell, and Ulf Ekelund. 2012. 
#' "Global Physical Activity Levels: Surveillance Progress, Pitfalls, and Prospects." 
#' The Lancet 380 (9838):247-57. https://doi.org/10.1016/S0140-6736(12)60646-1.
#' @export

calcPhysicalInactivity<-function(){
  dev<-calcOutput("DevelopmentState",aggregate = FALSE)
  dem<-calcOutput("Demography",aggregate = FALSE,education=FALSE)
  
  inactivity<-add_dimension(x = dev,add = "sex",nm = "M")
  inactivity<-add_columns(x=inactivity,addnm = "F",dim=3.1)
  
  #assumptions based on
  # http://apps.who.int/gho/data/view.main.2487?lang=en
  inactivity[,,"M"] = dev * 0.277 + (1-dev)*0.107
  inactivity[,,"F"] = dev * 0.376 + (1-dev)*0.224
  
  observations<-readSource("WHO","physical_inactivity_adults")/100
  observedcountries <- where(!is.na(observations))$true$region
  #plot(observations[observedcountries,2010,"M"]~inactivity[observedcountries,2010,"M"][,,"AG1"][,,"SSP2"])
  #calibrate to observations
  inactivity[observedcountries,,] = inactivity[observedcountries,,] + setYears(observations[observedcountries,2010,] - inactivity[observedcountries,2010,],NULL)
  
  inactivity[inactivity<0.03]<- 0.03 # set reasonable bounds
  inactivity[inactivity>0.97]<- 0.97 # set reasonable bounds
  
  inactivity2=dem*0+1
  inactivity2=inactivity2*inactivity[,getYears(dem),getNames(dem,dim=1)]
  # informed assumption based on Hallal et al 
  #a=x/(1.5^0.5) and b=x*(1.5^0.5) results in the effect that a/b=2/3
  working=c( "20--24","25--29","30--34","35--39","40--44","45--49","50--54","55--59" )
  retired=c("60--64","65--69","70--74","75--79","80--84","85--89","90--94","95--99")
  inactivity2[,,working]=inactivity2[,,working]/(1.5^0.5) 
  inactivity2[,,retired]=inactivity2[,,retired]*(1.5^0.5)
  
  # physical inactivity set to 80% across all incomes for kids, based on
  # http://apps.who.int/gho/data/view.main.2487ADO?lang=en
  underaged=c("0--4","5--9","10--14","15--19")
  inactivity2[,,underaged][,,"M"]<-0.776
  inactivity2[,,underaged][,,"F"]<-0.839
  
  observations<-readSource("WHO","physical_inactivity_underaged")/100
  observedcountries <- where(!is.na(observations))$true$region
  #plot(observations[observedcountries,2010,"M"]~inactivity[observedcountries,2010,"M"][,,"AG1"][,,"SSP2"])
  #calibrate to observations
  inactivity2[observedcountries,,underaged] = setYears(observations[observedcountries,2010,] ,NULL)
  
  getSets(inactivity2)=c("region","year","scenario","sex","age")
  
  #adult=c("AG1","AG2","AG3","AG4")
  #x=dimSums((inactivity2[,,adult]*dem[,,adult]),dim=c(3.2,3.3,3.4))/dimSums(dem[,,adult],dim=c(3.2,3.3,3.4))
  #x[is.nan(x)]<-0
  #plotcountrymap(x[,"y2010","SSP2"],catMethod=c(0.1,0.15,0.2,0.25,0.3,0.35,0.4))
  
  return(list(x=inactivity2,
              weight=dem,
              unit="kcal per capita per day",
              description="Intake estimate based on normalized body weights",
              min=0,
              max=1
  )
  ) 
}