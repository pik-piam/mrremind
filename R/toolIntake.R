#' @title toolIntake
#' @description it computes the food intake pro capita through the bodyweight 
#' and the activity level. First it computes the basal metabolic rate (bmr) through 
#' the Schofield equation and then the estimated energy required (eer) depending on 
#' the activitiy level by FAO/WHO/UNU tables (Human Energy Requirments, Rome 2004) 
#' @param bodyweight bodyweight in kg per capita or "standardized" for assuming standard values
#' @param bodyheight for mehthod FAO_WHO_UNU1985
#' @param tmean mean annual temperature
#' @param method method for calculating intake: either FAO_WHO_UNU1985 for estimates based on height and bodyweight, schofield for just bodyweight, or HHS_USDA for recommended values for US-americans
#' @param inactivity the activity level and the bodyweight computed by the readHic2015 function
#' @author Eleonora Martinelli
#' @export


toolIntake <- function(bodyweight,bodyheight=NULL,inactivity,tmean=NULL, method=NULL) { 
  schoolkids=c("5--9","10--14","15--19")
  
  if(method=="FAO_WHO_UNU1985"){
    
    PAL = inactivity * 1.53 + (1-inactivity)* 1.76 # 2 being between active and vigourous
    # kids have a different inactivity criteria, so we increased physical activity for active kids
    PAL[,,schoolkids]  = inactivity[,,schoolkids]  * 1.53 + (1-inactivity[,,schoolkids] )* 1.76
    
    BMR=PAL*0
    BMR<-dimOrder(BMR,perm=c(2,3,1))
    
    ### Schofield equation Basal Metabolic Rate###
    ### bmr = S(age,sex)*weight + C(age,sex) ###
    
    parameters<-calcOutput("RegressionParameters",aggregate = FALSE,regression="FAO_WHO_UNU1985")
    
    BMR= collapseNames(bodyweight * parameters[,,"weight"] +bodyheight/100 * parameters[,,"height"] +  parameters[,,"intercept"],collapsedim = c(3,4,5))
    requirement=BMR*PAL
    
  } else if(method=="HHS_USDA") {    
    
    requirement = readSource("HHS_USDA",convert=FALSE)
    weight=requirement*0+1
    mapping=toolMappingFile(type = "sectoral",readcsv = T,name = "HHS_USDA2hic.csv")
    requirement=speed_aggregate(x = requirement,rel = mapping,weight=weight,from = "HHS_USDA",to = "hic",dim = 3.2)
    
    standardized_requirement = collapseNames(requirement[,,"Sedentary"])*inactivity+collapseNames(requirement[,,"Active"])*(1-inactivity)
    ### for kids, inactivity is defined differently (more than 60 minutes of physical activity)
    standardized_requirement[,,schoolkids] = collapseNames(requirement[,,"Moderately_active"][,,schoolkids])*inactivity[,,schoolkids]+collapseNames(requirement[,,"Active"][,,schoolkids])*(1-inactivity[,,schoolkids])
    
    requirement=standardized_requirement
  } else if(method=="schofield") {
    
    # FAO/WHO/UNU. 2004. Human Energy Requirements. 
    # http://www.fao.org/tempref/docrep/fao/007/y5686e/y5686e00.pdf.
    # http://www.fao.org/docrep/007/y5686e/y5686e07.htm
    
    PAL = inactivity * 1.53 + (1-inactivity)* 1.76 # 2 being between active and vigourous
    # kids have a different inactivity criteria, so we increased physical activity for active kids
    PAL[,,schoolkids]  = inactivity[,,schoolkids]  * 1.53 + (1-inactivity[,,schoolkids] )* 1.76
    
    BMR=PAL*0
    BMR<-dimOrder(BMR,perm=c(2,3,1))
    
    ### Schofield equation Basal Metabolic Rate###
    ### bmr = S(age,sex)*weight + C(age,sex) ###
    
    schofield<-calcOutput("RegressionParameters",aggregate = FALSE,regression="Schofield")
    
    BMR= collapseNames(bodyweight * schofield[,,"slope"] + schofield[,,"intercept"],collapsedim = c(3,4))
    requirement=BMR*PAL
    
  } else if(method=="Froehle") {
    
    PAL = inactivity * 1.53 + (1-inactivity)* 1.76 # 2 being between active and vigourous
    # kids have a different inactivity criteria, so we increased physical activity for active kids
    PAL[,,schoolkids]  = inactivity[,,schoolkids]  * 1.53 + (1-inactivity[,,schoolkids] )* 1.76
    
    BMR=PAL*0
    BMR<-dimOrder(BMR,perm=c(2,3,1))
    
    ### Schofield equation Basal Metabolic Rate###
    ### bmr = S(age,sex)*weight + C(age,sex) ###
    
    parameters<-calcOutput("RegressionParameters",aggregate = FALSE,regression="Froehle")
    
    
    
    BMR= collapseNames(bodyweight * parameters[,,"weight"] + tmean * parameters[,,"tmean"] +  parameters[,,"intercept"],collapsedim = c(3,4,5))
    requirement=BMR*PAL
    
  } else {stop("unknown method")}
  

  return(requirement)
}