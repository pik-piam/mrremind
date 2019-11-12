#' @title readNCDrisc
#' @description Reads in data from the NCD risc consortium
#' body height:
#' Collaboration (NCD-RisC), NCD Risk Factor. 2016. "A Century of Trends in Adult Human Height." ELife 5 (July):e13410. https://doi.org/10.7554/eLife.13410.
#' @param subtype "height" for body height data
#' @author Benjamin Leon Bodirsky
#' 
#' @return magpie object with the dataset downloaded. It contains missing values
#' and it is possible to replace them with the function convertNCDrisc
#' 
#' @seealso
#' \code{\link{convertNCDrisc}}


readNCDrisc <- function(subtype) 
{
  if (subtype=="height") {
    a<-read.csv("NCD_RisC_eLife_2016_height_age18_countries.csv")
    a<-a[,which(dimnames(a)[[2]]%in%c("ISO","Sex","Year.of.birth","Mean.height..cm."))]
    dimnames(a)[[2]]<-c("region","sex","year","height")
    a$sex <- gsub(pattern = "Men",replacement = "M",x = a$sex)
    a$sex <- gsub(pattern = "Women",replacement = "F",x = a$sex)
    out<-as.magpie(a)
  }else if(subtype=="BMI") {
    
    a<-read.csv("NCD_RisC_Lancet_2017_mean_BMI_female_age_specific_country_estimates.csv")
    a$Country=toolCountry2isocode(a$Country,mapping = c("China (Hong Kong SAR)"="HKG","dr congo"="COD", "macedonia (tfyr)"="MKD", "micronesia (federated states of)"="FSM"))
    a<-a[,c("Country","Year","Age.group","Mean.body.mass.index")]
    a$Year=paste0("y",a$Year)
    dimnames(a)[[2]]<-c("iso","year","age","BMI")
    BMI_female<-add_dimension(collapseNames(as.magpie(a,spatial=1,temporal=2)),dim = 3.2,add = "sex",nm = "F")
    
    a<-read.csv("NCD_RisC_Lancet_2017_mean_BMI_male_age_specific_country_estimates.csv")
    a$Country=toolCountry2isocode(a$Country,mapping = c("China (Hong Kong SAR)"="HKG","dr congo"="COD", "macedonia (tfyr)"="MKD", "micronesia (federated states of)"="FSM"))
    a<-a[,c("Country","Year","Age.group","Mean.body.mass.index")]
    a$Year=paste0("y",a$Year)
    dimnames(a)[[2]]<-c("iso","year","age","BMI")
    BMI_male<-add_dimension(collapseNames(as.magpie(a,spatial=1,temporal=2)),dim = 3.2,add = "sex",nm = "M")
    
    a<-read.csv("underaged_NCD_RisC_Lancet_2017_mean_BMI_female_age_specific_country_estimates.csv")
    a$Country=toolCountry2isocode(a$Country,mapping = c("China (Hong Kong SAR)"="HKG","dr congo"="COD", "macedonia (tfyr)"="MKD", "micronesia (federated states of)"="FSM"))
    a<-a[,c("Country","Year","Age.group","Mean.body.mass.index")]
    a$Year=paste0("y",a$Year)
    a$Age.group=paste0("age",a$Age.group)
    dimnames(a)[[2]]<-c("iso","year","age","BMI")
    BMI_underaged_female<-add_dimension(collapseNames(as.magpie(a,spatial=1,temporal=2)),dim = 3.2,add = "sex",nm = "F")
    
    a<-read.csv("underaged_NCD_RisC_Lancet_2017_mean_BMI_male_age_specific_country_estimates.csv")
    a$Country=toolCountry2isocode(a$Country,mapping = c("China (Hong Kong SAR)"="HKG","dr congo"="COD", "macedonia (tfyr)"="MKD", "micronesia (federated states of)"="FSM"))
    a<-a[,c("Country","Year","Age.group","Mean.body.mass.index")]
    a$Year=paste0("y",a$Year)
    a$Age.group=paste0("age",a$Age.group)
    dimnames(a)[[2]]<-c("iso","year","age","BMI")
    BMI_underaged_male<-add_dimension(collapseNames(as.magpie(a,spatial=1,temporal=2)),dim = 3.2,add = "sex",nm = "M")
    
    eins<-mbind(BMI_female,BMI_male)
    zwei<-mbind(BMI_underaged_male,BMI_underaged_female)
    out<-mbind(zwei,eins)
    '    
    a<-read.csv("NCD_RisC_Lancet_2017_BMI_child_adolescent_country.csv")
    a<-a[,which(dimnames(a)[[2]]%in%c("ISO","Sex","Year","Mean.BMI"))]
    dimnames(a)[[2]]<-c("region","sex","year","BMI")
    a$sex <- gsub(pattern = "Boys",replacement = "M",x = a$sex)
    a$sex <- gsub(pattern = "Girls",replacement = "F",x = a$sex)
    out<-as.magpie(a)
    '
    
  } else if (subtype=="BMI_shr"){
  
    out=NULL
    
    for(sex in c("M","F")){
      for(BMI in c("BMI_18-5","BMI_18-5_20","BMI_20_25","BMI_25_30","BMI_30_35","BMI_35_40","BMI_40")){
        if(sex=="M"){long="male"} else {long="female"}
        a<-read.csv(paste0("NCD_RisC_Lancet_2017_prevalence_",BMI,"kgm2_", long,"_age_specific_country_estimates.csv"))
        a$Country=toolCountry2isocode(a$Country,mapping = c("China (Hong Kong SAR)"="HKG","dr congo"="COD", "macedonia (tfyr)"="MKD", "micronesia (federated states of)"="FSM"))
        a<-a[,c("Country","Year","Age.group",names(a)[3])]
        a$Year=paste0("y",a$Year)
        dimnames(a)[[2]]<-c("iso","year","age",BMI)
        new<-add_dimension(add_dimension(collapseNames(as.magpie(a,spatial=1,temporal=2)),dim = 3.2,add = "sex",nm = sex),dim = 3.3,add = "BMI_shr",  nm = BMI)
        out<-mbind(out,new)
      }
    }
  } else if (subtype=="BMI_shr_underaged"){
    
    out=NULL
    
    for(sex in c("M","F")){
      for(BMI in c("BMI_2sd","BMI_1sd_2sd","BMI_minus1sd_1sd","BMI_minus1sd_minus2sd","BMI_minus2sd")){
        if(sex=="M"){long="male"} else {long="female"}
        a<-read.csv(paste0("underaged_NCD_RisC_Lancet_2017_prevalence_",BMI,"_",long,"_age_specific_country_estimates.csv"))
        a$Country=toolCountry2isocode(a$Country,mapping = c("China (Hong Kong SAR)"="HKG","dr congo"="COD", "macedonia (tfyr)"="MKD", "micronesia (federated states of)"="FSM"))
        a<-a[,c("Country","Year","Age.group",names(a)[3])]
        a$Year=paste0("y",a$Year)
        a$Age.group=paste0("age",a$Age.group)
        dimnames(a)[[2]]<-c("iso","year","age",BMI)
        new<-add_dimension(add_dimension(collapseNames(as.magpie(a,spatial=1,temporal=2)),dim = 3.2,add = "sex",nm = sex),dim = 3.3,add = "BMI_shr",  nm = BMI)
        out<-mbind(out,new)
      }
    }
    
  }
  return(out)
}
