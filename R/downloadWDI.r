#' Dowload WDI
#' 
#' Download WDI (World development indicators) data .rda file. 
#' the WDI data is updated with the funciton "WDISearch(cache=WDIcache())"
#' 
#' @author  Xiaoxi Wang
#' @seealso  \code{\link{downloadSource}} \code{\link{WDI}}
#' @examples
#' 
#' \dontrun{ a <- downloadSource(type="WDI")
#' }
#' 
#' @importFrom reshape2 melt
#' @importFrom WDI WDI WDIcache WDIsearch


downloadWDI<-function(){
  end_year <- as.numeric(strsplit(as.character(Sys.Date()),"-")[[1]][1]) -1
  WDIsearch(cache = WDIcache())
  indicator <- c("SP.POP.TOTL", #population, total
                 "NY.GDP.MKTP.PP.KD",
                 "NY.GDP.MKTP.PP.CD","NY.GDP.MKTP.CD",
                 "NY.GDP.MKTP.KD","NY.GDP.MKTP.KN","NV.AGR.TOTL.ZS",
                 "NV.AGR.TOTL.KD","SP.URB.TOTL.IN.ZS","AG.SRF.TOTL.K2",
                 "NY.GDP.PCAP.CN", #GDP per capita current LCU,
                 "NY.GDP.PCAP.PP.KD", #GDP per capita PPP, 2011int$,
                 "NY.GDP.PCAP.KD", #GDP per capita MER, 2010 US$,
                 "NV.AGR.TOTL.KD", #Ag GDP MER, 2010 US$
                 "NY.GDP.PCAP.CD", #GDP per capita, current US$
                 "NY.GDP.PCAP.PP.CD" #GDP per capita, current PPP int$
                 )
  wdi <- WDI(indicator = indicator,start= 1960, end = end_year)
  save(wdi,file = paste("WDI","rda",sep="."))
}



