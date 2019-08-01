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
  indicator <- c("SP.POP.TOTL","NY.GDP.MKTP.PP.KD","NY.GDP.MKTP.PP.CD","NY.GDP.MKTP.CD","NY.GDP.MKTP.KD",
                 "NY.GDP.MKTP.KN","NV.AGR.TOTL.ZS","NV.AGR.TOTL.KD","SP.URB.TOTL.IN.ZS","AG.SRF.TOTL.K2")
  wdi <- WDI(indicator = indicator,start= 1960, end = end_year)
  save(wdi,file = paste("WDI","rda",sep="."))
}



