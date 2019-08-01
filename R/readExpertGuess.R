#' Read ExpertGuess
#' 
#' Read-in data that are based on expert guess
#' 
#' 
#' @param subtype Type of data that should be read. 
#' @return magpie object of the data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="ExpertGuess",subtype="ies")
#' }
#' 
readExpertGuess<-function(subtype){
  

  if (subtype == "ies"){
    a <- read.csv("ies.csv",sep = ";")
  } else if (subtype == "prtp"){
    a <- read.csv("prtp.csv",sep = ";")
  } else if (subtype == "CCSbounds"){
    a <- read.csv("CCSbounds.csv",sep = ";")
  } else if (subtype == "co2prices"){
    a <- read.csv("co2prices.csv",sep = ";")
  } else if (subtype == "costsTradePeFinancial"){
    a <- read.csv("pm_costsTradePeFinancial.csv",sep = ";",skip=2)  
  }
  
  if (subtype == "ies" | subtype == "prtp" | subtype == "CCSbounds" | subtype == "co2prices") {
    a$RegionCode <- NULL
    a$Country    <- NULL
    out <- as.magpie(a)
  } else if (subtype == "costsTradePeFinancial"){
    out <- as.magpie(a,spatial=1,temporal=0,datacol=3)
    out <- collapseNames(out)
#    getSets(out) <- c("region","year","type","pe")
  }  
    
  if (subtype == "ies" | subtype == "prtp"){
    getYears(out) <- "2005"
  } 

  return(out)
}
