#' Calculate Water Consumption Coefficients
#' 
#' This function calculates REMIND input data on water consumption coefficients
#' per electricity technology, using as initial information the Macknick (2011)
#' data per electricity technology. The source data provide most required
#' information but some assumptions on missing data are also made.
#' 
#' 
#' @return MAgPIE object on water consumption coefficients per elecricity
#' technology
#' @author Ioanna Mouratiadou
#' @seealso \code{\link{calcOutput}}, \code{\link{readMacknickIntensities}},
#' \code{\link{calcWaterWithCoef}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("WaterConsCoef")
#' 
#' }
#' 
calcWaterConsCoef <- function() {
  
  # read in data
  data <- readSource("MacknickIntensities","data")
  
  #use data for median withdrawal
  data <- collapseNames(data[,,"Water consumption median (gal/MWh)",drop=FALSE])

  #convert from gal/MWh into m3/MWh
  data <- data * 0.0037854 
  
  # read in mapping to REMIND technologies
  map_table <- read_excel(toolGetMapping(type = "sectoral", name = "TechnologyMappingMacknick2REMIND.xlsx",
                                         returnPathOnly = TRUE, where = "mappingfolder"))
  map <- list()
  map$macknick <- paste(map_table$'Macknick Source',map_table$'Macknick Technology',map_table$'Macknick Cooling',sep=".")
  map$remind <- paste(map_table$`REMIND Technology`,map_table$`REMIND Cooling`,sep=".")
  
  # calculate REMIND input in REMIND categories
  output <- new.magpie(getRegions(data),getYears(data),map$remind)
  output[,,] <- 0
  for(d in 1:length(map$macknick)){
    if( !map$macknick[d] == "-.-.-"){
      output[,,map$remind[d]] <- data[,,map$macknick[d]]
    }
  }
  
  # add some assumed data
  assudata <- readSource("MacknickIntensities","missingAssumed")
  assudata <- collapseNames(assudata[,,"Water consumption median (gal/MWh)",drop=TRUE],1)
  assudata <- assudata * 0.0037854 
  
  # add assumed data to data (by overrighting)
  output[,,getNames(assudata)] <- assudata[,,getNames(assudata)]
  output[,,"ngcc.once"]     <- 0.66 * output[,,"ngcc.once"] + 0.34 * output[,,"dot.once"]
  output[,,"ngcc.tower"]    <- 0.66 * output[,,"ngcc.tower"] + 0.34 * output[,,"dot.tower"]  
  output[,,"ngcc.pond"]    <- 0.66 * output[,,"ngcc.pond"] + 0.34 * output[,,"dot.pond"]    
  output[,,"ngcc.dry"]    <- 0.66 * output[,,"ngcc.dry"] + 0.34 * output[,,"dot.dry"]      
  output[,,"gaschp.once"]     <- 0.50 * output[,,"dot.once"]
  output[,,"gaschp.tower"]    <- 0.50 * output[,,"dot.tower"]
  output[,,"gaschp.pond"]     <- 0.50 * output[,,"dot.pond"]
  output[,,"gaschp.dry"]      <- 0.50 * output[,,"dot.dry"]
  output[,,"coalchp.once"]   <- 0.50 * output[,,"pc.once"]
  output[,,"coalchp.tower"]  <- 0.50 * output[,,"pc.tower"]
  output[,,"coalchp.pond"]   <- 0.50 * output[,,"pc.pond"]
  output[,,"coalchp.dry"]    <- 0.50 * output[,,"pc.dry"]
  output[,,"biochp.once"]   <- 0.50 * output[,,"pc.once"]
  output[,,"biochp.tower"]  <- 0.50 * output[,,"pc.tower"]
  output[,,"biochp.pond"]   <- 0.50 * output[,,"pc.pond"]
  output[,,"biochp.dry"]    <- 0.50 * output[,,"pc.dry"]
  
  return(list(x=output,weight=NULL,
              unit="m3/MWh", 
              description="Water Consumption coefficients for different electricity and cooling technologies based on Macknick et al. (2011) report",
              isocountries=FALSE
  ))
}
