#' Convert IFA
#' 
#' Convert IFADavies (2013) data to ISO country level.
#' 
#' 
#' @param x MAgPIE object containing IFA data region resolution
#' @return MAgPIE object of the IFA data disaggregated to country level
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertIFA(x)
#' }
#' 
#' @importFrom magclass where
#' @importFrom magpiesets findset

convertIFA <- function(x) {
  
  #delete all non-grand-totals
  x<-x[,,c("Grand Total N","Grand Total P2O5","Grand Total K2O")]
  
  # delete dots in names as they cause problems with dimensions
  getNames(x) <- gsub("\\.","",getNames(x))

  mapping<-toolMappingFile(type="regional",name = "transition_mapping_IFA.csv",readcsv = T)
  
  # add 0 for azerbaijan in 1990, 
  vcat(verbosity = 1,"azerbaijan is set to 0 in 1990 to allow for toolISOhistorical")
  x["AZE",,"Grand Total P2O5"]<-0
  x["SRB",1994:1995,"Grand Total K2O"]=setYears(x["SRB",1996,"Grand Total K2O"],NULL)
  x["BIH",1994,]=setYears(x["BIH",1996,],NULL)
  x["SVN",1993,]=NA
  
  # convert historical data to countries with ISO-codes
  x <- toolISOhistorical(x,mapping = mapping)
  
  # disaggregate the Others* regions of the data
  mapping <- "regionmappingIFA.csv"
  m <- read.csv2(mapping)
  weight = calcOutput("Population",aggregate=FALSE)[as.character(m$CountryCode),2010,"pop_SSP2"]
  x_Others <- toolAggregate(x[as.character(unique(m$RegionCode)),,], mapping, weight=weight)
  # add countries of Others*-Regions 
  out <- mbind(x,x_Others)
    
  out <- toolCountryFill(out,fill=0,no_remove_warning=c("XOA", "XEA", "XOO", "XWA", "XAC"))
  
  vcat(verbosity = 2, paste("Incomplete fertilizer data in",paste(where(is.na(out))$true$regions,collapse=" ")))
  vcat(verbosity = 1, paste("Incomplete fertilizer data in 5year timesteps in",paste(where(is.na(out[,findset("past"),]))$true$regions,collapse=" ")))
  vcat(verbosity = 1, paste("Incomplete fertilizer data in 5year timesteps in 2010 in ",paste(where(is.na(out[,"y2010",]))$true$regions,collapse=" "),", filled with 0s."))
  
  out[is.na(out)]<-0
  out=out/1000
  
  return(out)
}
