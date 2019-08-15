#' @title readGSOC
#' @description This function reads the raw GSOC data (available at http://54.229.242.119/GSOCmap/) 
#' or if available the preprocessed raster layers. 
#' 
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#' 
#' @examples
#' \dontrun{ readSource("GSOC") }
#' 
#' @importFrom raster raster aggregate res projectRaster writeRaster
#' @importFrom magclass as.magpie mbind


readGSOC <- function() {
  
  if(!file.exists("GSOC_30cm.grd")) {
    
    tmp     <- raster("GSOCmapV1.2.0.tif")
    out     <- aggregate(tmp, fact=60, fun=mean)
    writeRaster(out, filename="GSOC_30cm.grd")
    
  } else {
    
    out <- raster("/home/kristine/mnt/rd3mod/inputdata/sources/GSOC/GSOC_30cm.grd")
  }
  
  lon <- seq(-179.75,179.75,by=0.5)
  lat <- rev(seq(-89.75,89.75,by=0.5))
  
  #Load celliso names for 1:59199 magpie cells
  mapping   <- toolMappingFile(type="cell",name="CountryToCellMapping.csv", readcsv=TRUE) 
  cellNames <- mapping$celliso
  
  #Change longitude and latitude
  r50     <- raster(res=0.5)
  toMag   <- projectRaster(out, r50, over=TRUE) #re-project to regular grid
  toMag   <- t(as.matrix(toMag))
  
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