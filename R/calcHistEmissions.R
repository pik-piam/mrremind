#' @title historical emissions per sector or mac
#' @description provides historical emissions values per economic sector or per mac sector. For now it only includes European data.
#'
#' @param subtype Either "sector" or "MAC"
#' @return magpie object of historical emissions data
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("HistEmissions")
#' }
#'  


calcHistEmissions <- function(subtype="sector"){
  
  if (subtype == "sector") {
    emi <- readSource(type="Eurostat",subtype="sectorEmi")[,seq(1998,2017,1),c("CO2","CH4","N2O")] # "GHG","CO2","CH4","N2O","HFC","PFC","HFC_PFC_NSP","SF6","NF3"
    getNames(emi,dim=1) <- c("co2","ch4","n2o")
    description <- "Historical emissions per sector"
  } else if (subtype == "MAC"){
    emi <- readSource(type="Eurostat",subtype="MACCemi")[,seq(1998,2017,1),]
    description <- "Historical emissions per MAC sector"
  }
   
  out <- new.magpie(cells_and_regions = getRegions(emi), years = c(2000,2005,2010,2015), names = getNames(emi))
  out[,2000,] <- dimSums(emi[,c(1998,1999,2000,2001,2002),],dim = 2)/5
  out[,2005,] <- dimSums(emi[,c(2003,2004,2005,2006,2007),],dim = 2)/5
  out[,2010,] <- dimSums(emi[,c(2008,2009,2010,2011,2012),],dim = 2)/5
  out[,2015,] <- dimSums(emi[,c(2013,2014,2015,2016,2017),],dim = 2)/5
  
  #Returning emission values
  return(list(x=out, weight=NULL,
              unit="Mt CO2eq", 
              description="Emissions per sector"             
  )) 
}

