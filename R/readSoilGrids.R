#' @title readSoilGrids
#' @description This function reads the raw SoilGrids data (available at https://files.isric.org/soilgrids/data/recent/OCSTHA_M_30cm_250m_ll.tif) 
#' or if available the preprocessed raster layers. 
#' 
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#' 
#' @seealso
#' \code{\link{downloadSoilGrids}}
#' 
#' @examples
#' \dontrun{ readSource("SoilGrids") }
#' 
#' @importFrom raster raster aggregate res projectRaster writeRaster
#' @importFrom magclass as.magpie mbind

readSoilGrids <- function() {
  
  if(!file.exists("SoilGrids_30cm.grd")) {

    tmp     <- raster("OCSTHA_M_30cm_250m_ll.tif")
    out     <- aggregate(tmp, fact=240, fun=mean)
    writeRaster(out, filename="SoilGrids_30cm.grd")
    
  } else {
  
    out <- raster("SoilGrids_30cm.grd")
  }
  
  lon <- seq(-179.75,179.75,by=0.5)
  lat <- rev(seq(-89.75,89.75,by=0.5))
  
  #Load celliso names for 1:59199 magpie cells
  mapping   <- toolMappingFile(type="cell",name="CountryToCellMapping.csv", readcsv=TRUE) 
  cellNames <- mapping$celliso
  
  #Change longitude and latitude 
  r50   <- raster(res=0.5)
  toMag <- projectRaster(out,r50,over=TRUE)
  toMag <- t(as.matrix(toMag))
  
  #Create array for 59199 isocells, 1 year and 1 data dimension
  mag   <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,"y2000","soilc"))
  
  #Fill array with data from raster object (note magpie_coord are loaded by default)
  for (j in 1:59199) {
    mag[j,,] <- toMag[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
  }
  
  #Convert array to magpie object and rename set
  x          <- clean_magpie(as.magpie(mag))
  getSets(x) <- c("cell","t","data")
  
  return(x)
}

