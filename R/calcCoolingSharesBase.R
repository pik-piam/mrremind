#' Calculate Cooling Type Shares for the Base Year
#' 
#' This function calculates REMIND input data for the shares of cooling types
#' per electricity technology and REMIND region in 2005, using as initial
#' information the Davies (2013) data per electricity technology and GCAM
#' region. The source data provide most required information but some
#' assumptions on missing data are also made.
#' 
#' 
#' @return MAgPIE object on cooling type shares per elecricity technology and
#' REMIND region
#' @author Lavinia Baumstark, Ioanna Mouratiadou
#' @seealso \code{\link{calcOutput}}, \code{\link{readDaviesCooling}},
#' \code{\link{convertDaviesCooling}},
#' \code{\link{calcCoolingSharesAll}},\code{\link{calcCoolingSharesFuture}}
#' @examples
#' 
#' \dontrun{ 
#' a <- calcOutput("CoolingSharesBase")
#' }
#' @importFrom magclass getNames<-
#' @importFrom readxl read_excel
#' @importFrom madrat toolGetMapping



calcCoolingSharesBase <- function() {
  
  # read in data
  data <- readSource("DaviesCooling", subtype="dataBase")
  getNames(data)[grepl("^Sea",getNames(data))] <- "Sea.NA"
  
  # seperate data for Sea water
  Sea <- data[,,"Sea"]
  data <- data[,,-which(getNames(data)=="Sea.NA")]
  
  id <- getNames(data,dim=1)
  
  # calculate pond
  pond <- new.magpie(getRegions(data),getYears(data),id)
  for (i in id) {
    pond[,,i] <- 100 - dimSums(data[,,i],dim=3.2)
  }
  getNames(pond) <- paste(getNames(pond),"Pond",sep=".")
  
  # add pond to data
  data <- mbind(data,pond)
  
  # calculate sea water
  sea_new <- new.magpie(getRegions(data),getYears(data),id)
  for (i in id) {
    sea_new[,,i] <- data[,,paste(i,"1-thru",sep=".")] * Sea/100
  }
  getNames(sea_new) <- paste(getNames(sea_new),"Sea",sep=".")
  
  # correct 1-thru-data
  data[,,"1-thru"]  <- data[,,"1-thru"] * (1 - Sea/100)
  # add sea data to data
  data <- mbind(data,sea_new)
  
  # check if all categories sum up to 100%
  check <- new.magpie(getRegions(data),getYears(data),id)
  for (i in id) {
    check[,,i] <- dimSums(data[,,i],dim=3.2)  
  }
  if(!all(check==100)) { stop("sum of categorie XXX is not 100%")}
  
  # read in mapping to REMIND technologies
  map_table <- read_excel(toolGetMapping(type = "sectoral",
                                         name = "TechnologyMappingDavies2REMIND.xlsx",
                                         returnPathOnly = TRUE, where = "mappingfolder"))
  map <- list()
  map$davies <- paste(map_table$'Davies Source/Technology',map_table$'Davies Cooling',sep=".")
  map$remind <- paste(map_table$'REMIND Technology',map_table$'REMIND Cooling',sep=".")
  
  # calculate REMIND input in REMIND categories
  output <- new.magpie(getRegions(data),getYears(data),map$remind)
  output[,,] <- 0
  for(d in 1:length(map$davies)){
    if( !map$davies[d] == "-.-"){
      output[,,map$remind[d]] <- data[,,map$davies[d]]
    }
  }
  
  # add assumed data
  output[,,"geohdr.tower"]  <- 70
  output[,,"geohdr.dry"]    <- 20
  output[,,"geohdr.hybrid"] <- 10
  output[,,"hydro.default"] <- 100
  output[,,"wind.default"]  <- 100
  output[,,"spv.default"]   <- 100
  output[,,"csp.tower"]     <- 70
  output[,,"csp.dry"]       <- 20
  output[,,"csp.hybrid"]    <- 10
  
  outputBase <- new.magpie(getRegions(output),c(2005),getNames(output))
  outputBase[,,] <- output[,,]
  
  #assign aggregation weight
  weight     <- dimSums(calcOutput("IO",subtype="output",aggregate=FALSE)[,2010,c("feelb","feeli")],dim=3) 
  
  #set weights to zero for countries that were not contained in the GCAM2ISO mapping
  weight["ALA",,] <- 0
  weight["ATA",,] <- 0
  weight["BES",,] <- 0
  weight["BLM",,] <- 0
  weight["CUW",,] <- 0
  weight["GGY",,] <- 0
  weight["IMN",,] <- 0
  weight["JEY",,] <- 0
  weight["MAF",,] <- 0
  weight["PSE",,] <- 0
  weight["SSD",,] <- 0
  weight["SXM",,] <- 0
  
  return(list(x=outputBase,weight=weight,
              unit="% of cooling type technologies", 
              description="Cooling shares for different cooling technologies based on Davies et al. (2013) publication and using electricity use weights (aggregated based on IEA World Energy Balances, 2014) for regional mapping"
  ))
}
