#' @title calcExcretionIPCC
#' @description calculates excretion in the year 2005 using the IPCC Guidelines excretion rates.
#'
#' @param products IPCC: IPCC products. MAgPIE: Magpie products
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @seealso
#' \code{\link{calcExcretion}},
#' \code{\link{calcAnimalStocks}}
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ExcretionIPCC")
#' }
#' 
#' @importFrom magpiesets findset



calcExcretionIPCC<-function(products="IPCC"){
  years<-findset("past")
  stocks<-calcOutput("AnimalStocks",aggregate = F)[,years,]
  stocks[stocks<0] <- 0
  tam<-setYears(collapseNames(readSource("IPCC","awmsParCh4")[,,"Mass"]),NULL)
  
  development<-calcOutput("DevelopmentState",aggregate = FALSE)[,years,"SSP2"]
  tam2<-setYears(readSource("IPCC",subtype="ch10_table10a9",convert=FALSE),NULL)
  tam2=collapseNames(collapseNames(tam2[,,"developed"])*development+collapseNames(tam2[,,"developing"])*(1-development))
  tam2<-add_columns(tam2,dim = 3.1,addnm = getNames(tam))
  tam2[,,getNames(tam)]<-tam
  
  tam<-tam2
  
  shr<-setYears(readSource("IPCC","awmsShr"),NULL)
  
  n_rate<-setYears(readSource("IPCC","nExcrRate")*365/1000000,NULL)
  
  animals<-getNames(tam)
  
  nex<-stocks[,,animals]*tam[,,animals]*n_rate[,,animals]
  excretion<-nex*shr
  
  if (products=="magpie"){
    map<-toolMappingFile(type = "sectoral",name="IPCCitems.csv",readcsv=T)
    excretion=toolAggregate(x = excretion,dim = 3.1,rel = map,from = "ipcc",to="magpie",partrel = T)
  } else if (products=="IPCC"){
    excretion=excretion
  } else {stop("product types have to be either IPCC or magpie")}
  
  return(list(x=excretion,
              weight=NULL,
              unit="Mt Nr",
              description="Excreted nitrogen per animal type and animal waste management system")
  )                   
}

