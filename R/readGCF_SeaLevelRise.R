#' @title readGCF_SeaLevelRise
#' @description Reads in an extreme sea level rise dataset from Daniel Linke of Global Climate Forum, 
#' original dataset a binary (0/1) 3arcsec grid with 1 indicating grid lost.
#' Scenario is high-end 1.7m global average sea level rise, with no adaptation (only current levels of dykes and dams).
#' agg.tif file is original grid aggregated to 0.5deg resolution (factor 600) by sum. 
#' @return A MAgPIE object, cellular 0.5deg resolution, of fraction of land lost to SLR by cell.
#' @importFrom raster raster


readGCF_SeaLevelRise <- function() {
  
  file <- "agg.tif"
  loss <- raster(file)  
  
    mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE) 
    cellNames <- mapping$celliso
    lon <- seq(-179.75,179.75,by=0.5)
    lat <- rev(seq(-89.75,89.75,by=0.5))

    loss <- loss/360000 #divide the aggregated sum by the factor (600*600) for percentage 
  
    loss1 <- loss
    loss1[is.na(loss1[])] <- 0
      
    r50   <- raster(res=0.5)
    toMag <- projectRaster(loss1,r50,over=TRUE)
    toMag[which(toMag[]<0)] <-0
    toMag <- t(as.matrix(toMag))
    
  
  
      mag <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,"y2100","percentage_lost"))
      for (j in 1:59199) {
        mag[j,,] <- toMag[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
      }


    
    x <- as.magpie(mag,spatial=1,temporal=2)

  return(x)
}
