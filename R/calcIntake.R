############################
#' @title calcIntake
#' @description it computes the total intake kcal/day procapita through the population dataset by Lutz 2014,
#' height and BMI data of NCDrisc, and phyiscal inactivity levels based on Halal et al.
#' @param convert if TRUE, Lutz data is converted (interpolated completed etc)
#' @param modelinput if TRUE, data is aggregated to country totals for model input
#' @param standardize if FALSE, no standardization. if "recommendations", the US recommendations are used. if BMI, a normal BMI is used.
#' @param method method for calculating intake: either FAO_WHO_UNU1985 for estimates based on height and bodyweight, schofield for just bodyweight, or HHS_USDA for recommended values for US-americans
#' @return  total "healthy" intake kcal/day procap for each countries divided by sex and 8 age groups.
#' @export


calcIntake <- function(convert=TRUE,modelinput=FALSE,standardize=FALSE,method="FAO_WHO_UNU1985"){
  ## to do for better transparency: delete scenario dimension of demo. but a bit complicated due to dimsums in this function and in toolIntake
  
  # population dataset by Lutz 2014 and bodyweight dataset by Hic 2015
  demo <- calcOutput("Demography",convert=convert,education=FALSE,aggregate = FALSE)
  
  inactivity = calcOutput("PhysicalInactivity",aggregate = FALSE)
  
  # calculating calory requirements and adding pregnancy bonus
  if(standardize==FALSE) {
    
    height<-calcOutput("BodyHeight",convert=convert,aggregate=FALSE)
    BMI<-readSource("NCDrisc",subtype="BMI",convert=convert)
    if(method=="Froehle"){
      tmean<-calcOutput("Temperature",landusetypes="urban",months=FALSE,aggregate = FALSE,convert=convert)
    }
    
    if (convert==FALSE){  # still adjust the format
      mapping<-toolMappingFile(type = "sectoral",name = "NCDrisc2Lutz.csv",readcsv = TRUE)
      tmp<-new.magpie(cells_and_regions = getRegions(BMI),years = getYears(BMI),names = c(paste0(unique(mapping$lutz),".M"),paste0(unique(mapping$lutz),".F")))
      for(i in getNames(tmp,dim=1)){
        item<-mapping$NCDrisc[mapping$lutz==i]
        tmp[,,i]=dimSums(BMI[,,item],dim="age")/length(item)
      }
      BMI<-tmp
    }
    
    if(convert==FALSE){
      #1965:2010 is the period with observations for both body height and BMI
      height<-time_interpolate(dataset = height,interpolated_year = 1975:2010,integrate_interpolated_years = FALSE)  
      BMI<-time_interpolate(dataset = BMI,interpolated_year = 1975:2010,integrate_interpolated_years = FALSE)  
      inactivity<-time_interpolate(dataset = inactivity,interpolated_year = 1975:2010,integrate_interpolated_years = FALSE)  
      demo<-time_interpolate(dataset = demo,interpolated_year = 1975:2010,integrate_interpolated_years = FALSE)  
      commonregions <- 
        intersect(getRegions(BMI),
                  intersect(getRegions(height),
                            intersect(getRegions(demo),getRegions(inactivity))
                  )
        )
      
      if(method=="Froehle"){
        commonregions <- intersect(commonregions, getRegions(tmean))
        tmean<-tmean[commonregions,,]
        tmean<-time_interpolate(dataset = tmean,interpolated_year = 1975:2010,integrate_interpolated_years = FALSE)  
      } 
      
      demo <- demo[commonregions,,]
      height <- height[commonregions,,]
      inactivity <- inactivity[commonregions,,]
      BMI <- BMI[commonregions,,]
    } else {
      # assume constant BMI before observation period
      height<-time_interpolate(dataset = height,interpolated_year = findset("past"),integrate_interpolated_years = FALSE,extrapolation_type = "constant")  
      BMI<-time_interpolate(dataset = BMI,interpolated_year = findset("past"),integrate_interpolated_years = FALSE,extrapolation_type = "constant")    
      inactivity<-time_interpolate(dataset = inactivity,interpolated_year = findset("past"),integrate_interpolated_years = FALSE,extrapolation_type = "constant")
      demo<-demo[,getYears(height),]
      if(method=="Froehle"){
        tmean<-tmean[,getYears(height),]
      }
    }

    
    weight = (height/100)^2*BMI
    getSets(weight)=c("region","year","sex","age")
    getSets(inactivity)=c("region","year","scenario","sex","age")
    getSets(demo)=c("region","year","scenario","sex","age")
    
    intk_procap <- toolIntake(bodyweight=weight,bodyheight=height,inactivity=inactivity,method=method,tmean=tmean)
  } else if (standardize=="recommendations"){
    if(method!="HHS_USDA"){stop("Method for this standadization type not available")}
    if(convert==FALSE) {
      commonregions <- intersect(getRegions(demo),getRegions(inactivity))
      demo <- demo[commonregions,,]
      inactivity <- inactivity[commonregions,getYears(demo),]
    }
    intk_procap <- toolIntake(bodyweight=NULL,inactivity=inactivity,method=method)  
  } else if (standardize=="BMI") {
    height<-calcOutput("BodyHeight",convert=convert,aggregate=FALSE)
    commonregions <- intersect(getRegions(demo),getRegions(inactivity))
    commonregions <- intersect(commonregions, getRegions(height))
    
    commonyears=intersect(getYears(height),getYears(inactivity))
    commonyears <- intersect(commonyears, getYears(height))
    commonyears <- intersect(commonyears, getYears(demo))
    
    if(method=="Froehle"){
      tmean<-calcOutput("Temperature",landusetypes="urban",months=FALSE,aggregate = FALSE,convert=convert)
      commonregions <- intersect(commonregions, getRegions(tmean))
      commonyears <- intersect(commonyears, getYears(tmean))
      tmean<-tmean[commonregions,commonyears,]
    } else {
      tmean <- NULL
    }
    demo<-demo[commonregions,commonyears,]
    inactivity<-inactivity[commonregions,commonyears,]
    height<-height[commonregions,commonyears,]
    
    weight = 21.75*(height/100)^2
    # for adolescents:
    #https://www.cdc.gov/nchs/data/series/sr_11/sr11_246.pdf
    weight[,,"0--4"] = 16*(height[,,"0--4"]/100)^2
    weight[,,"5--9"] = 16*(height[,,"5--9"]/100)^2
    weight[,,"10--14"] = 18*(height[,,"10--14"]/100)^2
    weight[,,"10--14"] = 18*(height[,,"10--14"]/100)^2
    weight[,,"15--19"] = 21*(height[,,"15--19"]/100)^2
    weight[,,"15--19"] = 21*(height[,,"15--19"]/100)^2
    

    intk_procap <- toolIntake(bodyweight=weight,bodyheight=height,inactivity=inactivity,tmean=tmean,method=method)
    

  } else {stop("unknown setting for standardize")}
  
  reproductive<-c("F.20--24","F.25--29","F.30--34","F.35--39")
  intk_procap[,,reproductive] <- intk_procap[,,reproductive] + toolPregnant(demo,reproductive)
  
  weight<-collapseNames(demo[,getYears(intk_procap),][,,c("M","F")]) + 0.0000001

  weight=add_columns(weight,addnm = "B",dim = 3.2)
  weight[,,"B"]<-dimSums(weight[,,"B",invert=TRUE],dim=3.2)
  intk_procap=add_columns(intk_procap,addnm = "B",dim = 3.1)
  Both<-dimSums(intk_procap[,,"B",invert=T]*weight[,,"B",invert=T],dim=3.1)/dimSums(weight[,,"B",invert=T],dim="sex")
  intk_procap[,,"B"]<-Both
  
  weight=add_columns(weight,addnm = "All",dim = 3.3)
  weight[,,"All"]<-dimSums(weight[,,"All",invert=TRUE],dim=3.3)
  
  intk_procap=add_columns(intk_procap,addnm = "All",dim = 3.2)
  All<-dimSums(intk_procap[,,"All",invert=T]*weight[,,"All",invert=T],dim=3.2)/dimSums(weight[,,"All",invert=T],dim="age")
  intk_procap[,,"All"]<-All
  
  if(modelinput==TRUE){
    intk_procap<-collapseNames(intk_procap[,,"All"][,,"B"])
    weight<-collapseNames(weight[,,"All"][,,"B"])
  } else  if(modelinput=="age_groups_hist") {
    past<-findset("past")
    intk_procap<-collapseNames(intk_procap[,past,"SSP2"])
    intk_procap<-intk_procap[,,"B",invert=TRUE]
    intk_procap<-intk_procap[,,"All",invert=TRUE]
    weight<-weight[,past,"SSP2"]
    weight<-weight[,,"B",invert=TRUE]
    weight<-weight[,,"All",invert=TRUE]
  } else if(modelinput!=FALSE) {stop("unknown setting for modelinput")}
  
  return(list(x=intk_procap,
              weight=weight,
              unit="kcal per capita per day",
              description="Intake estimate",
              min=500,
              max=5000,
              isocountries=convert
  )
  ) 
}
