############################
#' @title calcIntakeByGroup
#' @description it computes the intake per capita for each BMI group, excluding intake for pregnancy or lactation
#' height and BMI data of NCDrisc, and phyiscal inactivity levels based on Halal et al.
#' @param convert if TRUE, Lutz data is converted (interpolated completed etc)
#' @param method method for calculating intake: either FAO_WHO_UNU1985 for estimates based on height and bodyweight, schofield for just bodyweight, or HHS_USDA for recommended values for US-americans
#' @return  total intake kcal/day procap for each countries divided by sex, age groups and BMI groups.
#' @export


calcIntakeByGroup <- function(convert=TRUE,method="schofield"){
  ## to do for better transparency: delete scenario dimension of demo. but a bit complicated due to dimsums in this function and in toolIntake
  
  # population dataset by Lutz 2014 and bodyweight dataset by Hic 2015
  
  inactivity = collapseNames(calcOutput("PhysicalInactivity",aggregate = FALSE)[,,"SSP2"])
  bodyheight<-calcOutput("BodyHeight",convert=convert,aggregate=FALSE)
  
  BMI<-readSource("WHObmi",convert=convert)
  
  if(method=="Froehle"){
    tmean<-calcOutput("Temperature",landusetypes="urban",months=FALSE,aggregate = FALSE,convert=convert)
  }
  
  if (convert==FALSE){  
    time_period=1961:2010
  } else { 
    time_period=findset("past")
  }
  
  #1965:2010 is the period with observations for both body height and BMI
  bodyheight = bodyheight[,time_period,]
  inactivity<-time_interpolate(dataset = inactivity,interpolated_year = time_period,integrate_interpolated_years = FALSE,extrapolation_type = "constant")  
  commonregions <- intersect(getRegions(bodyheight),getRegions(inactivity))
  
  
  if(method=="Froehle"){
    commonregions <- intersect(commonregions, getRegions(tmean))
    tmean<-tmean[commonregions,,]
    tmean<-time_interpolate(dataset = tmean,interpolated_year = time_period,integrate_interpolated_years = FALSE,extrapolation_type = "constant")  
  } 
  
  bodyheight <- bodyheight[commonregions,,]
  inactivity <- inactivity[commonregions,,]
  
  bodyweight = (bodyheight/100)^2*BMI
  getSets(inactivity)=c("region","year","sex","age")
  
  intk_procap <- toolIntake2(bodyweight=bodyweight,bodyheight=bodyheight,inactivity=inactivity,method=method,tmean=tmean)
  
  return(list(x=intk_procap,
              weight=NULL,
              unit="kcal per capita per day",
              description="Intake estimate",
              min=500,
              max=5000,
              isocountries=FALSE  ## has to be aggregated by calcIntake
  )
  ) 
}
