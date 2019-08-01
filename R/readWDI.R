#' Read WDI
#' 
#' Read-in WDI (World development indicators) data .rda file as magclass
#' object.
#' 
#' 
#' @param subtype Type of WDI data that should be read. Please use the
#' worldbank indicator abbreviation. Available types are e.g.: \itemize{ \item
#' \code{SP.POP.TOTL}: Population, total \item \code{NY.GDP.MKTP.PP.KD}: GDP,
#' PPP (constant 2011 international Dollar) \item \code{NY.GDP.MKTP.PP.CD}:
#' GDP, PPP (current international Dollar) \item \code{NY.GDP.MKTP.CD}: GDP MER
#' (current USDollar) \item \code{NY.GDP.MKTP.KD}: GDP MER (constant 2005
#' USDollar) \item \code{NY.GDP.MKTP.KN}: GDP LCU (constant LCU) \item \code{SP.URB.TOTL.IN.ZS}: Urban population (peccentage of total)}
#' @return magpie object of the WDI data
#' @author Jan Phillip Dietrich, Benjamin Bodirsky, Xiaoxi Wang
#' @seealso \code{\link{readSource}} \code{\link{downloadWDI}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="WDI",subtype="SP.POP.TOTL")
#' }
#' 

readWDI<-function(subtype){
  load("WDI.rda")
  if (subtype == "NE.CON.PRVT.PP.CD"){
    # Some values for Chinese Household Consumption Expenditure are not reported in the newer version (will probably be corrected later)
    # Reintroduce the values shown in the csv file
    wdi[wdi$country == "China" & wdi$Year %in% c(1990:2009), "NE.CON.PRVT.PP.CD"] <- c( 3.87E+11, 4.42E+11, 5.19E+11, 5.88E+11, 
                                                                                        6.47E+11, 7.39E+11, 8.40E+11, 9.09E+11,
                                                                                        9.89E+11, 1.10E+12, 1.24E+12, 1.36E+12,
                                                                                        1.49E+12, 1.64E+12, 1.84E+12, 2.09E+12,
                                                                                        2.40E+12, 2.75E+12, 3.13E+12, 3.47E+12 )
  }
  wdi$country <- NULL
  wdi <- as.magpie(melt(wdi,id.vars = c("iso2c","year")),spatial=1,temporal=2,tidy=TRUE,replacement =".")
  wdi <- wdi[,,subtype]
  return(wdi)
}
