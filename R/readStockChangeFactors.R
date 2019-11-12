#' @title readStockChangeFactors
#' @description Read in IPCC stock cange factors for lpj croptypes
#' @return List of magpie objects with results on cellular level
#' @author Kristine Karstens
#' @seealso
#' \code{\link{readStockChangeFactors}},
#' @examples
#' 
#' \dontrun{ 
#' readSource("StockChangeFactors")
#' }
#'
#' @importFrom magclass read.magpie
#' @importFrom lpjclass readLPJ
#' @importFrom lpjclass read.LPJ_input 
#' @importFrom lpjclass as.lpj 
#' @importFrom utils tail data


readStockChangeFactors<-function(){
  
  fourClimateZones <- NULL
  load("climData.RData")
  climate <- array(fourClimateZones, dim=c(67420,1,1,1))
  
  lpjclassdata          <- NULL
  data("lpjclassdata", envir = environment(), package="lpjclass")
  land                   <- lpjclassdata$cellbelongings[,c("LPJ.Index","country.code")]
  climate                <- climate[which(lpjclassdata$grid_67420_59199==1),,,,drop=FALSE]
  dimnames(climate)[[1]] <- paste(land$countryname,1:59199,sep=".")
  climate                <- as.magpie(as.lpj(climate))
  
  getNames(climate) <- "tropical"
  climate <- add_columns(climate, addnm=c("subtropical","temperate","boreal"))
  climate[,,"boreal"] <- climate[,,"temperate"] <- climate[,,"subtropical"] <- climate[,,"tropical"]
  
  for(i in c(1:4)){
    
    toReplace <- climate[,,i]
    toReplace[toReplace!=i] <- 0
    toReplace[toReplace==i] <- 1
    climate[,,i]    <- toReplace
  }
  
  SCF_climate  <- read.magpie("StockChangeFactors.csv")
  SCF_cellular <- dimSums(climate * SCF_climate, dim=3.1)
  
  dimnames(SCF_cellular)[[1]] <- paste0("GLO.",1:dim(SCF_cellular)[1])
  SCF_cellular <- clean_magpie(SCF_cellular)
  
  return(SCF_cellular)
}