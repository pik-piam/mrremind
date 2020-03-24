#' Calculate baseline emissions for maccs
#' 
#' Provides REMIND data for baseline emissions for maccs.
#' 
#' 
#' @return REMIND data for baseline emissions for maccs and corresonding
#' weights (NULL) as a list of two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("calcEmiMac")
#' }
#' @importFrom magclass getNames<- getYears<-

calcEmiMac <- function() {
  
  # emissions for the calculation of econometric paramter p1
  co2       <- readSource("EDGAR",subtype="co2") * 12/44 * 1e-6
  
  n2o       <- readSource("EDGAR",subtype="n2owaste")  * 28/44 * 1e-3 
  ch4       <- readSource("EDGAR",subtype="ch4waste") * 1e-3   
   
  co2cement <- dimSums(co2[,,c("2A1","2A2")],dim=3)
  getNames(co2cement) <- "co2cement_process"
  co2luc <- dimSums(co2[,,c("5A","5D","5F2")],dim=3)
  getNames(co2luc) <- "co2luc"
  n2owaste <- dimSums(n2o[,,c("6A","6B","6C","6D")],dim=3)
  getNames(n2owaste) <- "n2owaste"
  n2otrans <- dimSums(n2o[,,c("1A3a","1A3b","1A3c","1A3d","1A3e")],dim=3)
  getNames(n2otrans) <- "n2otrans"
  n2oacid <- dimSums(n2o[,,c("2B")],dim=3)
  getNames(n2oacid) <- "n2oacid"
  ch4wsts  <- dimSums(ch4[,,c("6B")],dim=3)
  getNames(ch4wsts) <- "ch4wsts"
  ch4wstl  <- dimSums(ch4[,,c("6A","6C","6D")],dim=3)
  getNames(ch4wstl) <- "ch4wstl"
  
  # overwritting european countries with eurostat data
  EUcountries <- c("ALA","AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FRO","FIN","FRA","DEU","GIB","GRC","GGY","HUN","IRL","IMN","ITA","JEY","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE","GBR")
  baselineEurostat <- calcOutput("HistEmissions",subtype="MAC",aggregate=F)
  n2owaste[EUcountries,2005,"n2owaste"] <- baselineEurostat[EUcountries,2005,"n2owaste"] 
  n2otrans[EUcountries,2005,"n2otrans"] <- baselineEurostat[EUcountries,2005,"n2otrans"]  
  n2oacid[EUcountries,2005,"n2oacid"] <- dimSums(baselineEurostat[EUcountries,2005,c("n2oadac", "n2onitac")])   
  ch4wsts[EUcountries,2005,"ch4wsts"] <- baselineEurostat[EUcountries,2005,"ch4wsts"] 
  ch4wstl[EUcountries,2005,"ch4wstl"] <- baselineEurostat[EUcountries,2005,"ch4wstl"] 
  
  # combine all parameters
  x <- mbind(co2cement,co2luc,n2owaste,n2otrans,n2oacid,ch4wsts,ch4wstl)
  getYears(x) <- NULL 
  return(list(x=x,
              weight=NULL,
              unit="GtC, MtCH4, MtN2O",
              description="emissions in 2005",
              note=c('used to calculate econometric emission parameter p1')))
}
