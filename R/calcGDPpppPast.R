#' calcGDPpppPast
#' 
#' Calculates a time series of GDP in Purchase Power Parity (PPP) of million
#' International Dollars of the year 2005.  The source is selected in the
#' config file (getConfig()$calc$GDPpppPast). Different sources are available:
#' \itemize{ \item \code{WDI}: The PPP estimate from the World Development
#' Indicators (WDI) are supplemented by values for Argentina, Syria and Somalia
#' which are missing in the database. The values were taken from World Bank.
#' 2014. Purchasing Power Parities and the Real Size of World Economies: A
#' Comprehensive Report of the 2011 International Comparison Program. The World
#' Bank. http://elibrary.worldbank.org/doi/book/10.1596/978-1-4648-0329-1.
#' table 2.13 Then, the 2011 estimate is extrapolated with the GDP projection
#' in local currency units (LCU), as these growth rates should be approximately
#' what the growth models predict. The price index from 2011 was transformed
#' into 2005 equivalents using the inflation rate of the United States (US), as
#' the PPPs are in USDollar equvialents. 
#' \item \code{PWT}: Penn World Tables
#' \item \code{IHME_USD05_PPP_pc}: Publication: James, Spencer L., Paul Gubbins,
#' Christopher JL Murray, and Emmanuela Gakidou. 2012. "Developing a
#' Comprehensive Time Series of GDP per Capita for 210 Countries from 1950 to
#' 2015."" Population Health Metrics 10 (1): 12. doi:10.1186/1478-7954-10-12.
#' This publication also contains further interpolated indicators that can be
#' used. }
#' 
#' @param GDPpppPast GDPppp past data source
#' @return GDP PPP in million USD05 equivalents
#' @author Lavinia Baumstark, Benjamin Bodirsky
#' @seealso \code{\link{convertWDI}}
#' @importFrom magclass getNames


calcGDPpppPast <- function(GDPpppPast="IHME_USD05_PPP_pc_completed") {
  type <- GDPpppPast
  if(type=="PWT"){
    data <- readSource("PWT")[,,"rgdpna"]
    getNames(data) <- "GDPppp_PWT"
  } else if (type=="DemandModel"){
    data <- collapseNames(readSource("DemandModel")[,,"gdp"])
    getNames(data) <- paste0("Pop_",getNames(data))
  } else if (type=="WDI"){
    
    LCU<-readSource("WDI","NY.GDP.MKTP.KN")
    # 2011 ppp dataset
    PPP<-readSource("WDI","NY.GDP.MKTP.PP.KD")
    
    #complete it with missing countries
    missingcountries<-c("ARG","SOM","SYR")
    if(any(!is.na(PPP[missingcountries,,]))) {
      stop("Database has been updated, please recheck whether replacement values are required")
    } else {
      warning("Filling values for Argentina, Somalia and Syria")
    } 
    # enter values from the publication
    #World Bank. 2014. Purchasing Power Parities and the Real Size of World Economies: A Comprehensive Report of the 2011 International Comparison Program. The World Bank. http://elibrary.worldbank.org/doi/book/10.1596/978-1-4648-0329-1.
    #table 2.13
    
    PPP[missingcountries,"y2011",] <- c(691.2*10^9,3*10^9,142.9*10^9)
    
    # esimate PPP as ICP2011 estimate with growth rate of GDP in local currencies
    PPP<-LCU[,,]/setYears(LCU[,"y2011",])*setYears(PPP[,"y2011",],NULL)
    
    # inflate it to get a 2005 estimate
    ppp_current<-readSource("WDI",subtype = "NY.GDP.MKTP.PP.CD")
    inflator<- setYears(ppp_current["USA","y2005",]/PPP["USA","y2005",],NULL) 
    
    data<-PPP*colSums(inflator,na.rm=T)
    getNames(data) <- "GDPppp05_WDI_ICP11"
  } else if (type%in%c("IHME_USD05_PPP_pc","IHME_USD05_MER_pc","IMF_USD05_PPP_pc","PENN_USD05_PPP_pc","WB_USD05_PPP_pc","MADDISON_USD05_PPP_pc","WB_USD05_MER_pc","IMF_USD05_MER_pc","UN_USD05_MER_pc")) {
    PPP_pc<-readSource(type="James",subtype = type)
    pop<-readSource("WDI",subtype = "SP.POP.TOTL")
    years<-intersect(getYears(PPP_pc),getYears(pop))
    data=PPP_pc[,years,]*pop[,years,]
    getNames(data) <- substr(type,1,(nchar(type)-3))
  } 
  
  else if (type %in% c("IHME_USD05_PPP_pc_completed","IHME_USD05_MER_pc_completed")){
    if(type=="IHME_USD05_PPP_pc_completed"){
      data <- calcOutput("GDPpppPast", GDPpppPast="IHME_USD05_PPP_pc",aggregate = FALSE)
    } else if (type=="IHME_USD05_MER_pc_completed") {
      data <- calcOutput("GDPpppPast", GDPpppPast="IHME_USD05_MER_pc",aggregate = FALSE)  
    }
    
    missing<-where(data==0)$true$region
    
    fill <- readSource("MissingIslands", subtype = "gdp", convert = FALSE)
    fill <- time_interpolate(fill,interpolated_year = getYears(data),extrapolation_type = "constant")
    
    missing<-where(setYears(dimSums(data,dim=2),"y2000")==0)$true$region
    data[missing,,]<-fill[missing,,]
    missing<-where(data==0)$true$region
    for(ii in missing){
      missingyears=where(data[ii,,]==0)$true$years
      data[ii,missingyears,] <- time_interpolate(dataset = data[ii,,][,missingyears,,invert=T],interpolated_year = missingyears,extrapolation_type = "constant")
    }
    
    getNames(data) <- "gdp"

  }
  else {
    stop(type, " is not a valid source type for GDP")
  }
  return(list(x=data,weight=NULL,unit="Million US Dollar 2005 equivalents in PPP",description="GDP in PPP with baseyear in 2005. PPP may come either from ICP 2005 or 2011."))
}
