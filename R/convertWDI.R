#' Convert WDI
#' 
#' Convert WDI converts data from readWDI() to ISO country level. Adds Taiwan
#' as difference from global total.
#' 
#' 
#' @param x MAgPIE object containing WDI data region resolution
#' @param subtype Name of the worldbank indicator, e.g. "SP.POP.TOTL"
#' @return MAgPIE object of the WDI data disaggregated to country level
#' @author Jan Phillip Dietrich, Benjamin Bodirsky, Xiaoxi Wang
#' @examples
#' 
#' \dontrun{ a <- convertWDI(x)
#' }
#' @importFrom magclass getCells<-
#' @importFrom countrycode countrycode


convertWDI<-function(x,subtype){
  WDI_data <- WDI::WDI_data
  
  # changing scale of indicators
  if (subtype %in% c("SP.POP.TOTL","NY.GDP.MKTP.PP.KD","NY.GDP.MKTP.PP.CD","NY.GDP.MKTP.CD","NY.GDP.MKTP.KD","NY.GDP.MKTP.KN")) {
    x <- x/1000000
    #Kosovo added to Serbia
    x["RS",,] <- dimSums(x[c("RS","XK"),,],dim=1,na.rm=T) 
    }else if (subtype %in%  WDI_data$series[,"indicator"]){
    # urbanisation rate and population density and land surface
    # include c("SP.URB.TOTL.IN.ZS", "EN.POP.DNST", "AG.SRF.TOTL.K2", "NE.CON.PRVT.PC.KD", "NE.CON.PRVT.PP.CD","NE.CON.PRVT.PP.KD")
    vcat("Warning: Kosovo left out of conversion and has differing population values from FAO", verbosity=2)
    x <- x
  }else {
    stop("subtype does not exist in the dataset!")
  }
  y <- x
  
  JG <- "JEY"
  names(JG) <- "JG"
  getCells(y)<-countrycode(getCells(y),"iso2c","iso3c", custom_match = JG)
  y<-y[!is.na(getCells(y)),,]
  y<-clean_magpie(y)
  
  y<-y["ANT",,,invert=TRUE]
  
  y<-toolCountryFill(y,fill = 0)
  y[is.na(y)]<-0
  y <- y[,sort(getYears(y)),]
  #remove years which only contain 0s as entries
  y <- y[,!apply(y,2,function(x) return(all(x==0))),]
  ## taiwan only listed in global totals, not explicetly
  #world<- colSums(y,na.rm=T)
  #taiwan<-x["1W",,] - world
  #y["TWN",,]<-colSums(taiwan,na.rm=T)
  y<-y[,sort(getYears(y)),]
  return(y)
}
