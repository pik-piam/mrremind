#' Read WHO
#' 
#' Read-in WHO (World health organization) data files as magpie object.
#' The files contain information on physical inactivity
#' 
#' 
#' @param subtype Type of WHO data that should be read. Includes
#' physical_inactivity_adults and physical_inactivity_underaged
#' @return magpie object of the WHO data
#' @author Benjamin Bodirsky
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="WHO",subtype="physical_activity_adults")
#' }
#' 
readWHO<-function(subtype){
  
  reformat<-function(x){
    x<-strsplit(x,split = " ")
    x<-unlist(lapply(x,FUN = function(x){return(x[[1]])}))
    x[x=="No"]<-NA
    x<-as.numeric(x)
    return(x)
  }
  
  if (subtype == "physical_inactivity_adults"){
    a<-read.csv("NCD_PAC,NCD_PAA.csv",stringsAsFactors = F)
  } else if (subtype == "physical_inactivity_underaged"){
    a<-read.csv("NCD_PAC_ADO.csv",stringsAsFactors = F)
  }
  
  a<-a[,c(1,2,5,6)]
  a<-a[-1,]
  colnames(a)=c("iso","year","M","F")
  
  a$M <- reformat(a$M)
  a$F <-  reformat(a$F)
  a$year <- sub(a$year,pattern = " ",replacement = "y")
  
  a$iso<-toolCountry2isocode(a$iso,mapping = c(
    "bolivia (plurinational state of)" = "BOL",
    "micronesia (federated states of)" ="FSM",
    "the former yugoslav republic of macedonia"="MKD",
    "united kingdom of great britain and northern ireland"="GBR",
    "venezuela (bolivarian republic of)"="VEN"
  ))
  out<-as.magpie(a)

  return(out)
}
