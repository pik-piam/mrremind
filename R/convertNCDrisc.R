#' @title convertNCDrisc
#' @description Converts data from the NCD risc consortium
#' body height:
#' Collaboration (NCD-RisC), NCD Risk Factor. 2016. "A Century of Trends in Adult Human Height." ELife 5 (July):e13410. https://doi.org/10.7554/eLife.13410.
#' @param x unconverted magpie object from read-script
#' @param subtype "height" for body height data. Missing data is replaced by non-population weighted global average
#' 
#' @return magpie object with a completed dataset.
#' 
#' @seealso
#' \code{\link{convertNCDrisc}}
#' 
#' @importFrom magpiesets findset


convertNCDrisc <- function(x, subtype) 
{
  if (subtype=="height"){
    meanheight=dimSums(x,dim=1)/length(getRegions(x))
    x<-toolCountryFill(x,fill = NA)
    countries<-where(is.na(x))$true$region
    x[countries,,]<-meanheight
    vcat(1, "better replace using function")
    
    out<-x
  } else if (subtype=="BMI"){
    mapping<-toolMappingFile(type = "sectoral",name = "NCDrisc2Lutz.csv",readcsv = TRUE)
    BMI<-new.magpie(cells_and_regions = getRegions(x),years = getYears(x),names = c(paste0(unique(mapping$lutz),".M"),paste0(unique(mapping$lutz),".F")))
    for(i in getNames(BMI,dim=1)){
      item<-mapping$NCDrisc[mapping$lutz==i]
      BMI[,,i]=dimSums(x[,,item],dim="age")/length(item)
    }
    
    meanBMI=dimSums(BMI,dim=1)/length(getRegions(BMI))
    BMI<-toolCountryFill(BMI,fill = NA)
    countries<-where(is.na(BMI))$true$region
    BMI[countries,,]<-meanBMI
    vcat(1, "better replace using function")
    out<-BMI
  } else if (subtype=="BMI_shr"){
    
    stop("use calcBMI_shr instead")
    #x<-readSource("NCDrisc",subtype = "BMI_shr",convert=FALSE)
    
    mapping<-toolMappingFile(type = "sectoral",name = "NCDriscBMIshr2Lutz.csv",readcsv = TRUE)
    BMI<-toolAggregate(x,rel = mapping,from = "NCDrisc",to = "lutz",dim = 3.1)
    
    withdata <- getRegions(BMI)
    BMI2<-toolCountryFill(BMI,fill = NA)
    BMI2<-add_columns(BMI2,dim = 2.1,addnm = c("y1965","y1970"))
    BMI2<-BMI2[,sort(getYears(BMI2)),]
    
    regression<-readSource("Bodirsky2018",convert = FALSE)
    
    gdp_pc<-collapseNames(calcOutput("GDPpc",gdp="PPP",aggregate = FALSE)[,,"SSP2"])
    bmi_regr=collapseNames(regression[,,"intercept"]+regression[,,"saturation"]*gdp_pc/(regression[,,"halfsaturation"]+gdp_pc))
    bmi_regr=time_interpolate(bmi_regr,interpolated_year = getYears(BMI2),integrate_interpolated_years = FALSE)
    
    mapping<-toolMappingFile(type = "sectoral",name = "Lutz2agegroups.csv",readcsv = TRUE)
    retired=mapping[mapping$agegroups=="retired",1]
    working=mapping[mapping$agegroups=="working",1]
    
    BMI2<-BMI2*NA
    BMI2[,,"BMI_18-5"][,,working] =     bmi_regr[,,"working"][,,"low"] * bmi_regr[,,"working"][,,"lowsplit"]
    BMI2[,,"BMI_18-5_20"][,,working] =  bmi_regr[,,"working"][,,"low"] * (1-bmi_regr[,,"working"][,,"lowsplit"])
    BMI2[,,"BMI_20_25"][,,working] =  (1-bmi_regr[,,"working"][,,"low"] - bmi_regr[,,"working"][,,"high"]) * (1-bmi_regr[,,"working"][,,"mediumsplit"])
    BMI2[,,"BMI_25_30"][,,working] =  (1-bmi_regr[,,"working"][,,"low"] - bmi_regr[,,"working"][,,"high"]) * (bmi_regr[,,"working"][,,"mediumsplit"])
    BMI2[,,"BMI_30_35"][,,working] =  bmi_regr[,,"working"][,,"high"] * (1-bmi_regr[,,"working"][,,"highsplit"])
    BMI2[,,"BMI_35_40"][,,working] =  bmi_regr[,,"working"][,,"high"] * bmi_regr[,,"working"][,,"highsplit"]
    BMI2[,,"BMI_40"][,,working] =  0 ## no data
    
    BMI2[,,"BMI_18-5"][,,retired] =     bmi_regr[,,"retired"][,,"low"] * bmi_regr[,,"retired"][,,"lowsplit"]
    BMI2[,,"BMI_18-5_20"][,,retired] =  bmi_regr[,,"retired"][,,"low"] * (1-bmi_regr[,,"retired"][,,"lowsplit"])
    BMI2[,,"BMI_20_25"][,,retired] =  (1-bmi_regr[,,"retired"][,,"low"] - bmi_regr[,,"retired"][,,"high"]) * (1-bmi_regr[,,"retired"][,,"mediumsplit"])
    BMI2[,,"BMI_25_30"][,,retired] =  (1-bmi_regr[,,"retired"][,,"low"] - bmi_regr[,,"retired"][,,"high"]) * (bmi_regr[,,"retired"][,,"mediumsplit"])
    BMI2[,,"BMI_30_35"][,,retired] =  bmi_regr[,,"retired"][,,"high"] * (1-bmi_regr[,,"retired"][,,"highsplit"])
    BMI2[,,"BMI_35_40"][,,retired] =  bmi_regr[,,"retired"][,,"high"] * bmi_regr[,,"retired"][,,"highsplit"]
    BMI2[,,"BMI_40"][,,retired] =  0 ## no data
    
    ### calibrate 1965 and 1970 using the divergence of 1975
    
    calib<-BMI[,"y1975",]-BMI2[withdata,"y1975",]
    
    BMI2[withdata,getYears(BMI),]<-BMI[withdata,getYears(BMI),]

    BMI2[withdata,c("y1965","y1970"),]=BMI2[withdata,c("y1965","y1970"),]+setYears(calib,NULL)
    # in case that calibration created negative values or values above one, remove them and add them to the middle category
    BMI2[BMI2<0]=0
    BMI2[BMI2>1]=1
    BMI2[,, "BMI_20_25" ]= BMI2[,, "BMI_20_25" ] + (1-dimSums(BMI2,dim=3.3))
    
    out<-BMI2
    
      
  } else if (subtype=="BMI_shr_underaged"){
    
    stop("use calcBMI_shr instead")
    #x<-readSource("NCDrisc",subtype = "BMI_shr_underaged",convert=FALSE)
    
    mapping<-toolMappingFile(type = "sectoral",name = "NCDriscBMIshrunderaged2Lutz.csv",readcsv = TRUE)
    names<-sort(apply(expand.grid(unique(mapping$lutz),getNames(x,dim="sex"),getNames(x,dim="BMI_shr")), 1, paste, collapse = ".", sep = "")) 
    BMI<-new.magpie(cells_and_regions = getRegions(x),years = getYears(x),names = names)
    for(i in getNames(BMI,dim=1)){
      item<-mapping$NCDrisc[mapping$lutz==i]
      BMI[,,i]=dimSums(x[,,item],dim="age")/length(item)
    }
    
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
    BMI2[,,"BMI_minus2sd"] =     bmi_regr[,,"underaged"][,,"low"] * bmi_regr[,,"underaged"][,,"lowsplit"]
    BMI2[,,"BMI_minus1sd_minus2sd"] =  bmi_regr[,,"underaged"][,,"low"] * (1-bmi_regr[,,"underaged"][,,"lowsplit"])
    BMI2[,,"BMI_minus1sd_1sd"] =  (1-bmi_regr[,,"underaged"][,,"low"] - bmi_regr[,,"underaged"][,,"high"]) 
    BMI2[,,"BMI_1sd_2sd"] =  bmi_regr[,,"underaged"][,,"high"] * (1-bmi_regr[,,"underaged"][,,"highsplit"])
    BMI2[,,"BMI_2sd"] =  bmi_regr[,,"underaged"][,,"high"] * bmi_regr[,,"underaged"][,,"highsplit"]

    ### calibrate 1965 and 1970 using the divergence of 1975
    
    calib<-BMI[,"y1975",]-BMI2[withdata,"y1975",]
    
    BMI2[withdata,getYears(BMI),]<-BMI[withdata,getYears(BMI),]
    
    BMI2[withdata,c("y1965","y1970"),]=BMI2[withdata,c("y1965","y1970"),]+setYears(calib,NULL)
    # in case that calibration created negative values or values above one, remove them and add them to the middle category
    BMI2[BMI2<0]=0
    BMI2[BMI2>1]=1
    BMI2[,, "BMI_minus1sd_1sd" ]= BMI2[,, "BMI_minus1sd_1sd" ] + (1-dimSums(BMI2,dim=3.3))
    BMI2<-add_columns(BMI2,addnm = "0--4",dim=3.1)
    BMI2<-BMI2[,,c("0--4","5--9","10--14","15--19")]
    BMI2[,,"0--4"]<-BMI2[,,"5--9"]
    out<-BMI2
  }
  return(out)
}
