#' @title calcBMIshr
#'
#' @description estimates population based on BMI share
#' 
#' @param convert Use raw data or interpolated data. Raw data should only be used for regressions.
#'
#' @return List with a magpie object
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{readNCDrisc}},
#' \code{\link{calcIntake}},
#' \code{\link{readKuczmarski}}
#' 
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("BMIshr")
#' }
#' 
#' 

calcBMIshr <- function(convert=TRUE){

  ### Adult
  
  x<-readSource("NCDrisc",subtype="BMI_shr",convert=FALSE)
  mapping<-toolMappingFile(type = "sectoral",name = "NCDriscBMIshr2Lutz.csv",readcsv = TRUE)
  x<-toolAggregate(x,rel = mapping,from = "NCDrisc",to = "lutz",dim = 3.1)
  mapping <- toolMappingFile(type = "sectoral",name = "BMIgroup_adultBMI.csv",readcsv = TRUE)
  adult<-toolAggregate(x,rel = mapping,from = "adultBMI",to = "BMIgroup",dim = 3.3,weight = NULL,partrel = TRUE)  
  
  ### underaged
  
  x<-readSource("NCDrisc",subtype="BMI_shr_underaged",convert=FALSE)
  
  ### aggregate to age groups, use age 5 for 0--4
  relevant=c("age5", "age6", "age7", "age8", "age9", "age10", "age11", "age12", "age13", "age14")
  
  x<-x[,,relevant]
  weight<-x*0+1  ### assume equal weighting within age groups
  mapping <- toolMappingFile(type = "sectoral",name = "NCDrisc2Lutz.csv",readcsv = TRUE)
  x<-toolAggregate(x,rel = mapping,from = "NCDrisc",to = "lutz",dim = 3.1,weight = weight,partrel = TRUE)  
  mapping <- toolMappingFile(type = "sectoral",name = "BMIgroup_underagedBMI.csv",readcsv = TRUE)
  x<-toolAggregate(x,rel = mapping,from = "underagedBMI",to = "BMIgroup",dim = 3.3,weight = NULL,partrel = TRUE)  
  underaged<-add_columns(x,addnm = c(  "mediumhigh"),dim=3.3)
  underaged[,,c( "mediumhigh")]=0
  ### 
  
  out<-mbind(adult,underaged)
  out<-out[,,getNames(adult,dim=3)] ###right order
  
  if (convert==TRUE) {
    BMI<-out
    withdata <- getRegions(BMI)
    BMI2<-toolCountryFill(BMI,fill = NA)
    BMI2<-add_columns(BMI2,dim = 2.1,addnm = c("y1965","y1970"))
    BMI2<-BMI2[,sort(getYears(BMI2)),]
    
    regression<-readSource("Bodirsky2018",convert = FALSE)
    
    gdp_pc<-collapseNames(calcOutput("GDPpc",gdp="PPP",aggregate = FALSE)[,,"SSP2"])
    
    bmi_regr=collapseNames(regression[,,"intercept"]+regression[,,"saturation"]*gdp_pc/(regression[,,"halfsaturation"]+gdp_pc))
    bmi_regr=time_interpolate(bmi_regr,interpolated_year = getYears(BMI2),integrate_interpolated_years = FALSE)
    
    mapping<-toolMappingFile(type = "sectoral",name = "Lutz2agegroups.csv",readcsv = TRUE)

    
    BMI2<-BMI2*NA
    
    for(agegroup in c("underaged","working","retired")){
      ages=mapping[mapping$agegroups==agegroup,1]
      BMI2[,,"verylow"][,,ages] =     bmi_regr[,,agegroup][,,"low"] * bmi_regr[,,agegroup][,,"lowsplit"]
      BMI2[,,"low"][,,ages] =  bmi_regr[,,agegroup][,,"low"] * (1-bmi_regr[,,agegroup][,,"lowsplit"])
      BMI2[,,"medium"][,,ages] =  (1-bmi_regr[,,agegroup][,,"low"] - bmi_regr[,,agegroup][,,"high"]) * (1-bmi_regr[,,agegroup][,,"mediumsplit"])
      BMI2[,,"mediumhigh"][,,ages] =  (1-bmi_regr[,,agegroup][,,"low"] - bmi_regr[,,agegroup][,,"high"]) * (bmi_regr[,,agegroup][,,"mediumsplit"])
      BMI2[,,"high"][,,ages] =  bmi_regr[,,agegroup][,,"high"] * (1-bmi_regr[,,agegroup][,,"highsplit"])
      BMI2[,,"veryhigh"][,,ages] =  bmi_regr[,,agegroup][,,"high"] * bmi_regr[,,agegroup][,,"highsplit"]
    }

    calib<-BMI[,"y1975",]-BMI2[withdata,"y1975",]
    
    BMI2[withdata,getYears(BMI),]<-BMI[withdata,getYears(BMI),]
    
    BMI2[withdata,c("y1965","y1970"),]=BMI2[withdata,c("y1965","y1970"),]+setYears(calib,NULL)
    # in case that calibration created negative values or values above one, remove them and add them to the middle category
    BMI2[BMI2<0]=0
    BMI2[BMI2>1]=1
    BMI2[,, "medium" ]= BMI2[,, "medium" ] + (1-dimSums(BMI2,dim=3.3))
    
    out<-BMI2   
    
  }
  
  weight <- collapseNames(calcOutput("Demography",aggregate=FALSE,education=FALSE)[,,"SSP2"])
  weight<-time_interpolate(weight,interpolated_year = getYears(out),extrapolation_type = "constant")

  return(list(x=out,
              weight=weight,
              unit="capita/capita",
              description="Share of population belonging to a BMI group.",
              isocountries=convert))
}