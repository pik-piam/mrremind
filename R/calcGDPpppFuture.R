#' calcGDPpppFuture
#' 
#' Calculates a time series of GDP in Purchase Power Parity (PPP) of million
#' International Dollars of the year 2005.  The source is selected in the
#' config file(getConfig()$calc$GDPpppFuture). Different sources are available:
#' \itemize{ \item \code{SSP}: Lavinia? \item \code{OECD}: Lavinia? }
#' 
#' @param GDPpppFuture GDPppp future data source
#' @return GDP PPP(ICP11) in million USD05 equivalents
#' @author Lavinia Baumstark, Benjamin Bodirsky
#' @seealso \code{\link{convertWDI}}
#' @importFrom magclass getNames setNames clean_magpie


calcGDPpppFuture <- function(GDPpppFuture="SSP_completed") {
  type <- GDPpppFuture
  if (type=="SRES"){type=c("sres_a1_gdp","sres_a2_gdp","sres_b1_gdp","sres_b2_gdp")}
  if(all(type%in%"OECD")){
    data <- readSource("OECD",subtype="gdp")*1000
    time_extend <- c("y2105","y2110","y2115","y2120","y2125","y2130","y2135","y2140","y2145","y2150")
    data <- time_interpolate(data,time_extend,extrapolation_type="constant",integrate_interpolated_years=TRUE)
    
  } else if (all(type%in%"SSP")){
    data <- collapseNames(readSource("SSP",subtype="all")[,,"GDP|PPP"][,,"OECD Env-Growth"])*1000
    # remove 2000 and 2005, because these years are not complete
    data <- data[,setdiff(getYears(data),c("y2000","y2005")),]
    getNames(data) <- paste("gdp_",gsub("_v[[:alnum:],[:punct:]]*","",getNames(data)),sep="")
    #remove years which only contain 0s     as entries
    data <- data[,!apply(data,2,function(x) return(all(x==0))),]
    time_extend <- c("y2105","y2110","y2115","y2120","y2125","y2130","y2135","y2140","y2145","y2150")
    data <- time_interpolate(data,time_extend,extrapolation_type="constant",integrate_interpolated_years=TRUE)
    
  } else if (all(type%in%c("sres_a1_gdp","sres_a2_gdp","sres_b1_gdp","sres_b2_gdp"))){
    vcat(1,"growth rates of SRES projections were multiplied on 1990 GDP of James et al")
    data<-NULL
    for (i in type) {
      data <- mbind(data,readSource(type="SRES",subtype=i))
    }
    getNames(data)<-paste0("gdp_",substr(getNames(data),6,7))
    PPP_pc<-readSource(type="James",subtype ="IHME_USD05_PPP_pc")
    pop<-readSource("WDI",subtype = "SP.POP.TOTL")
    years<-intersect(getYears(PPP_pc),getYears(pop))
    calib=PPP_pc[,years,]*pop[,years,]
    getNames(calib) <- "IHME_USD05_PPP"
    data <- data*setYears(setNames(calib[,"y1990",],NULL)/data[,"y1990",],NULL)
    time_extend <- c("y2105","y2110","y2115","y2120","y2125","y2130","y2135","y2140","y2145","y2150")
    data <- time_interpolate(data,time_extend,extrapolation_type="constant",integrate_interpolated_years=TRUE)
    data[is.na(data)]<-0
  } else if (type %in% c("SRES_completed","SSP_completed")){
    if (type=="SSP_completed") {
      data <- calcOutput("GDPpppFuture", GDPpppFuture="SSP",  aggregate = F)  
      fill <- readSource("MissingIslands", subtype = "gdp", convert = FALSE)
    } else if (type=="SRES_completed") {
      data <- calcOutput("GDPpppFuture", GDPpppFuture="SRES",  aggregate = F)
      fill <- calcOutput("GDPpppFuture", GDPpppFuture="SSP_completed",  aggregate = F)[,,"gdp_SSP2"]
    }
    
    missing<-where(data==0)$true$region
    fill <- time_interpolate(fill,interpolated_year = getYears(data),extrapolation_type = "constant")
    
    missing<-where(setYears(dimSums(data,dim=2),"y2000")==0)$true$region
    data[missing,,]<-fill[missing,,]
    missing<-where(data==0)$true$region
    for(ii in missing){
      missingyears=where(data[ii,,]==0)$true$years
      data[ii,missingyears,] <- time_interpolate(dataset = data[ii,,][,missingyears,,invert=T],interpolated_year = missingyears,extrapolation_type = "constant")
    }
    
  }
  
  else{
    stop(type, " is not a valid source type for gdp")
  }
  data<-clean_magpie(data)
  # put in alphabetical order
  data<-data[,,order(getNames(data))]
  return(list(x=data,weight=NULL,unit="Million US Dollar 2005 equivalents in PPP",description="US Dollar 2005 equivalents in PPP"))
}

