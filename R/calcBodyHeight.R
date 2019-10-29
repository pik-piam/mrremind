#' @title calcBodyHeight
#' @description reads in the Lutz et al dataset. 
#' Aggregates the age structure. Population is divided 
#' by sex male (M) , female (F) and both (B)
#' and divided by 8 age classes: 
#' 0-4, 5-9, 10-14, 15-19, AG1 (20-29), AG2 (30-59), AG3(60-79), AG4(80+) 
#' @param convert if TRUE, the convertscript of Lutz et al is activated.
#' Also,the year 1965 is extrapolatedusing the worldbank population 
#' data and sex, age, and education structure of 1970. 
#' @export
#' @importFrom luscale speed_aggregate

calcBodyHeight<-function(convert=TRUE){

  demo <- calcOutput("Demography",education=FALSE,aggregate=FALSE)
  demo <- collapseNames(demo[,,"SSP2"])
  demo <- time_interpolate(demo,interpolated_year = 1961:2010)
  
  height <- readSource("NCDrisc",subtype="height",convert=convert)
  height<-time_interpolate(height,interpolated_year = 1890:2014,extrapolation_type = "constant")
  if(!convert){
    demo<-demo[getRegions(height),,]
  }
  out<-demo*0
  
  #5_year_age_cohorts
  for(year_x in getYears(out,as.integer = TRUE)) {
    for(cohort in getNames(out,dim=2)){
      if (cohort=="100+"){
        low<-100
      }else {
        low=as.integer(strsplit(x=cohort,split = c("--"))[[1]])[1]  
      }
      low=year_x-low
      if (low<1892) {low=1892}
      if (low>1996) {low=1996}
      
      out[,year_x,cohort]<- dimSums(height[,low:(low+4),],dim=2)/5
    }
  }
  
  ## Use WHO growth standards (http://www.who.int/growthref/tools/en/)
  
  # calculate ratio relative to normal growth for 18 year olds
  ratio = collapseNames(out[,,"15--19"])
  ratio[,,"F"] = ratio[,,"F"]/163
  ratio[,,"M"] = ratio[,,"M"]/176
  
  out[,,"0--4"][,,"F"] <- 91
  out[,,"0--4"][,,"M"] <- 92
  out[,,"5--9"][,,"F"] <- 124
  out[,,"5--9"][,,"M"] <- 125
  out[,,"10--14"][,,"F"] <- 154
  out[,,"10--14"][,,"M"] <- 152
  
  # apply ratio to underaged
  out[,,c("0--4","5--9","10--14")] = out[,,c("0--4","5--9","10--14")] * ratio

  
  return(list(
    x=out,
    weight=demo,
    min=0,
    max=250,
    unit="cm per person",
    description="Bodyheight by age and sex",
    isocountries=convert
  ))
  
}
