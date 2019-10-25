#' @title calcDemography
#' @description reads in the Lutz et al dataset. 
#' Aggregates the age structure. Population is divided 
#' by sex male (M) , female (F) and both (B)
#' and divided by 8 age classes: 
#' 0-4, 5-9, 10-14, 15-19, AG1 (20-29), AG2 (30-59), AG3(60-79), AG4(80+) 
#' @param convert if TRUE, the convertscript of Lutz et al is activated.
#' Also,the year 1965 is extrapolatedusing the worldbank population 
#' data and sex, age, and education structure of 1970. 
#' @param education if FALSE, no education dimension will be provided
#' @export
#' @importFrom magpiesets findset

calcDemography<-function(convert=TRUE,education=TRUE){
  
  lutz <- readSource("Lutz2014",convert=convert)
  
  #demo <- groupAggregate(data = demo,query = mapping,from = "lutz", to="hic", dim=3.3)
  mapping2 <- toolMappingFile(type = "sectoral",name = "lutz2hic2.csv",readcsv = TRUE)
  demo <- speed_aggregate(x = lutz,rel = mapping2,from = "lutz", to="hic", dim=3.2)
  
  demo<-demo[,,"B",invert=T]
  demo<-demo[,,"All",invert=T]
  demo<-demo[,,"Total",invert=T]
  
  getSets(demo)<-c("region","year","scenario","sex","age","education")
  
  
  # in 2010 there are tiny differences in the demography. We still want to have a harmonized
  # dataset for 2010.
  past<-findset("past")
  demo[,intersect(getYears(demo),past),]<-demo[,intersect(getYears(demo),past),"SSP2"]

  
  # test for differences in population and demography datasets
  
  if(convert==TRUE){
    population<-collapseNames(calcOutput("Population",aggregate = FALSE,naming="indicator.scenario")[,,getNames(demo,dim = "scenario")])
    diff <- dimSums(demo,dim=c("sex","age","education")) - population[,getYears(demo),]
    diff[]<-abs(diff)
    if(sum(diff)>100){
      vcat(2, paste0(
        "Population and Demography datasets diverge: ",
        round(mean(dimSums(diff[,intersect(getYears(diff),findset("past")),],dim=c(1,3))/dim(diff)[3])),
        " mio per year for the past and ",
        round(mean(dimSums(diff[,setdiff(getYears(diff),findset("past")),],dim=c(1,3))/dim(diff)[3])),
        "mio per year for the future. Largest divergences in ",
        where(diff==max(diff))$true$regions[1],
        "in the year ",
        where(diff==max(diff))$true$year[1],
        " with ",
        round(diff[which(diff==max(diff),arr.ind = T)])[1],
        " million people."
      ))
    }
    
    # recalibration to SSP population scenarios
    demo_shr=demo/dimSums(demo,dim=c("sex", "age","education"))
    vcat(verbosity = 2, paste0("Year 1965 in demography data missing. Used values of 1970 instead"))
    demo_shr<-mbind(
      setYears(demo_shr[,"y1970",],"y1965"),
      demo_shr
    )
    demo_shr<-toolHoldConstantBeyondEnd(demo_shr)
    demo=demo_shr*population
  }

    
  
  if(!education){
    demo<-dimSums(demo,dim="education")
  }
  
  

  return(list(
    x=demo,
    weight=NULL,
    min=0,
    max=20000,
    unit="million people",
    description="Population by age, sex and education",
    isocountries=convert
  ))
  
}
