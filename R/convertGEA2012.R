#' @title convertGEA2012
#' @description Converts oil, gas and coal data from the Global Energy Assessment 2012 to country-level aggregation
#' @param x MAgPIE object to be disaggregated
#' @param subtype Type of fossil fuel (oil, coal or gas)
#' @return MAgPIE object containing country-level disaggregation of GEA 2012 data
#' @author Stephen Bi
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("GEA2012")
#' }
#' 

convertGEA2012 <- function(x,subtype) {
  if (subtype=='coal') {
    #Load mapping file for GEA regions to country level
    mapping <- toolGetMapping("regionmappingREMIND.csv","regional", where = "mappingfolder")
    #Load country-level BGR data on coal combined reserve & resource distribution to serve as a disaggregation weight
    w <- read.csv(paste0(getConfig("sourcefolder"),"/BGR/coal_reserves.csv"),header=TRUE,sep=";")[,c("Land_Region","Remaining_Potential")]
    #convert the data into a magpie object, convert countries to ISO code and set missing countries to 0
    w <- as.magpie(w,spatial=1,temporal=0,datacol=2)
    getRegions(w) <- toolCountry2isocode(getRegions(w))
    w <- toolNAreplace(toolCountryFill(w,fill=0))[[1]]
    #Disaggregate GEA coal data to country level based on the BGR weights
    out <- toolAggregate(x[,,'xi3'],mapping,w)
    #Cost data xi1 and xi2 kept constant across regions
    out <- mbind(out,toolAggregate(x[,,c('xi1','xi2')],mapping,weight=NULL))
  }else if (subtype %in% c('oil','gas')) {
    #Load mapping file for GEA regions to country level
    mapping <- toolGetMapping("regionmappingGEA2012.csv","regional")
    mapping$RegionCode[which(mapping$RegionCode=="ARC")] <- "WEU"
    mapping$RegionCode[which(mapping$RegionCode=="SOO")] <- "LAC"
    # Divide ARC fuels equally among EUR (WEU), USA, RUS (FSU), CAN
    # UNCLOS not ratified by USA, and territorial dispute would be uncertain even if it were
    for (reg in c("WEU", "USA", "FSU", "CAN")) {
      x[reg,,"xi3"] <- x[reg,,"xi3"] + 0.25 * x["ARC",,"xi3"]
    }
    x <- x[getRegions(x)!="ARC",,]  #Remove ARC region
    # Antarctic Treaty banned resource extraction until at least 2048
    x <- x[getRegions(x)!="SOO",,]  #Remove SOO region
    
    #Read country-level BGR data, distinguished between reserves and resources
    w <- read.csv(paste0(getConfig("sourcefolder"),"/BGR/",subtype,"_reserves.csv"),header=TRUE,sep=";")[,c("Land_Region","Reserves","Resources")]
    #Remove NAs
    w[is.na(w)] <- 0
    #Convert to magpie for use as a disaggregation weight, convert countries to ISO code and set missing countries to 0
    w <- as.magpie(w,spatial=1,temporal=0,datacol=2)
    getRegions(w) <- toolCountry2isocode(getRegions(w))
    w <- toolNAreplace(toolCountryFill(w,fill=0))[[1]]
    #Disaggregate the GEA data according to the BGR data on country-level oil/gas combined reserves + resources
    w <- dimSums(w,dim=3)
    out <- toolAggregate(x[,,getNames(x[,,'xi3'])],mapping,weight=w)
    #Cost data xi1 and xi2 kept constant across regions
    out <- mbind(out,toolAggregate(x[,,c('xi1','xi2','dec')],mapping,weight=NULL))
  }
  
  return(out)
}
