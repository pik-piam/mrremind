############################
#' @title calcIntake2
#' @description it computes the total intake kcal/day procapita through the population dataset by Lutz 2014,
#' height and BMI data of NCDrisc, and phyiscal inactivity levels based on Halal et al.
#' @param convert if TRUE, Lutz data is converted (interpolated completed etc)
#' @param modelinput if TRUE, data is aggregated to country totals for model input
#' @param standardize if FALSE, no standardization. if "recommendations", the US recommendations are used. if BMI, a normal BMI is used.
#' @param method method for calculating intake: either FAO_WHO_UNU1985 for estimates based on height and bodyweight, schofield for just bodyweight, or HHS_USDA for recommended values for US-americans
#' @return  total "healthy" intake kcal/day procap for each countries divided by sex and 8 age groups.
#' @export


calcIntake2 <- function(convert=TRUE,modelinput=FALSE,standardize=FALSE,method="schofield"){

  IntakeByGroup = calcOutput("IntakeByGroup",convert=convert,method=method,aggregate = FALSE)
  
  demo <- collapseNames(calcOutput("Demography",convert=convert,education=FALSE,aggregate = FALSE)[,,"SSP2"])
  
  BMI_shr<-calcOutput("BMIshr",convert=convert,aggregate = FALSE)
    
  if(standardize==TRUE){
    BMI_shr[,,]<-0
    BMI_shr[,,"medium"]=1
  }
  
  if(convert==FALSE){
    time_period=1975:2010
  } else {
    time_period=findset("past")
  }
 
  ### bring all in same format
  commonregions=Reduce(intersect, list(getRegions(IntakeByGroup),getRegions(demo),getRegions(BMI_shr)))
  IntakeByGroup<-IntakeByGroup[commonregions,time_period,]
  BMI_shr<-BMI_shr[commonregions,time_period,]
  demo<-  time_interpolate(demo[commonregions,,],interpolated_year = time_period,extrapolation_type = "constant",integrate_interpolated_years = FALSE)
  
  demo=demo*BMI_shr
  getSets(demo)=c("country","year","sex","age","BMIgroup")
  getSets(IntakeByGroup)=c("country","year","sex","age","BMIgroup")
  
  ### Add calories fro pregnancy and lactation
  reproductive<-c("F.20--24","F.25--29","F.30--34","F.35--39")
  IntakeByGroup[,,reproductive] <- IntakeByGroup[,,reproductive] + toolPregnant2(demo,reproductive)
  
  weight<-collapseNames(demo[,,c("M","F")]) + 0.0000001

  weight=add_columns(weight,addnm = "B",dim = 3.1)
  weight[,,"B"]<-dimSums(weight[,,"B",invert=TRUE],dim=3.1)
  IntakeByGroup=add_columns(IntakeByGroup,addnm = "B",dim = 3.1)
  Both<-dimSums(IntakeByGroup[,,"B",invert=T]*weight[,,"B",invert=T],dim=3.1)/dimSums(weight[,,"B",invert=T],dim=3.1)
  IntakeByGroup[,,"B"]<-Both
  
  weight=add_columns(weight,addnm = "All",dim = 3.2)
  weight[,,"All"]<-dimSums(weight[,,"All",invert=TRUE],dim=3.2)
  IntakeByGroup=add_columns(IntakeByGroup,addnm = "All",dim = 3.2)
  All<-dimSums(IntakeByGroup[,,"All",invert=T]*weight[,,"All",invert=T],dim=3.2)/dimSums(weight[,,"All",invert=T],dim=3.2)
  IntakeByGroup[,,"All"]<-All
  
  weight=add_columns(weight,addnm = "Entire",dim = 3.3)
  weight[,,"Entire"]<-dimSums(weight[,,"Entire",invert=TRUE],dim=3.3)
  IntakeByGroup=add_columns(IntakeByGroup,addnm = "Entire",dim = 3.3)
  All<-dimSums(IntakeByGroup[,,"Entire",invert=T]*weight[,,"Entire",invert=T],dim=3.3)/dimSums(weight[,,"Entire",invert=T],dim=3.3)
  IntakeByGroup[,,"Entire"]<-All
  
  if(modelinput==TRUE){
    IntakeByGroup<-collapseNames(IntakeByGroup[,,"All"][,,"B"][,,"Entire"])
    weight<-collapseNames(weight[,,"All"][,,"B"][,,"Entire"])
  } else  if(modelinput=="age_groups_hist") {
    past<-findset("past")
    IntakeByGroup<-collapseNames(IntakeByGroup[,past,"SSP2"])
    IntakeByGroup<-IntakeByGroup[,,"B",invert=TRUE]
    IntakeByGroup<-IntakeByGroup[,,"All",invert=TRUE]
    IntakeByGroup<-IntakeByGroup[,,"Entire",invert=TRUE]
    weight<-weight[,past,"SSP2"]
    weight<-weight[,,"B",invert=TRUE]
    weight<-weight[,,"All",invert=TRUE]
    weight<-weight[,,"Entire",invert=TRUE]
  } else if(modelinput!=FALSE) {stop("unknown setting for modelinput")}
  
  return(list(x=IntakeByGroup,
              weight=weight,
              unit="kcal per capita per day",
              description="Intake estimate",
              min=500,
              max=5000,
              isocountries=convert
  )
  ) 
}
