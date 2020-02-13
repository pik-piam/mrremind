#' @title readMIRCA
#' @description Read in data based on MIRCA data (https://www.uni-frankfurt.de/45218023/MIRCA)
#'  
#' @param subtype 'multicropping' (default): multicropping factor on cropped (excluding fallow) land, 
#'                'growing_period': area, start, end for 26 different crops (9 subcrops and 2 irrigation regime (irrigated, rainfed))
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' readSource("MIRCA", subtype="multicropping", convert="onlycorrect")
#' }
#'
#' @importFrom magclass read.magpie
#' @importFrom raster raster extent aggregate

readMIRCA<-function(subtype="multicropping"){
  
  if(subtype=="multicropping"){
    
    # Load MIRCA data at 1/12 resolution including 
    map.fallow           <- NULL     # fallow land                
    map.mca_annuals      <- NULL     # physical area of annuals 
    maps.mirca_ha_harmon <- NULL     # harvested area for various annual and perennial crops 
    load("maps_ha_and_fallow.RData")
    cat("Data was preprocessed before hand. Full data processing based on raw MIRCA data not yet implemented.")
    
    lon       <- seq(-179.75,179.75,by=0.5)
    lat       <- rev(seq(-89.75,89.75,by=0.5))
    mapping   <- toolGetMapping(type="cell",name="CountryToCellMapping.csv") 
    cellNames <- mapping$celliso
    
    #physical area
    physical.area         <- raster(t(map.mca_annuals[,2160:1]))
    extent(physical.area) <- c(-180,180,-90,90)
    physical.area         <- aggregate(physical.area,fact=6,fun=sum)
    
    # harvested area
    annuals <- c("Wheat","Maize","Rice","Barley","Rye","Millet","Sorghum","Soybeans","Sunflower",
                 "Potatoes","Cassava","Sugar Beet","Rapeseed","Groundnuts","Pulses","Cotton","Others Annual") 
    
    harvested.area         <- rowSums(maps.mirca_ha_harmon[,,annuals,], dims=c(2,3))
    harvested.area         <- raster(t(harvested.area[,2160:1]))
    extent(harvested.area) <- c(-180,180,-90,90)
    harvested.area         <- aggregate(harvested.area,fact=6,fun=sum)
    multicropping <- harvested.area/physical.area
    multicropping <- t(as.matrix(multicropping))
    
    to_mag <- multicropping
    #Create array for 59199 isocells, 1 year and 1 data dimension
    mag    <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,"y2000","multicropping"))
    
    #Fill array with data from raster object (note magpie_coord are loaded by default)
    for (j in 1:59199) {
      mag[j,,] <- to_mag[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
    }
    
    #Convert array to magpie object and rename set
    x          <- clean_magpie(as.magpie(mag))
    getSets(x) <- c("cell","t","variable")
    
  } else if(subtype=="growing_period"){
    
    grow_p  <- subset(read.csv("CELL_SPECIFIC_CROPPING_CALENDARS_30MN.TXT", sep = '\t',header = TRUE, fileEncoding = "us-ascii"), 
                      select=c("row","column","crop","subcrop","area","start","end"))
    
    mirca_crops           <- read.csv("readme__growing_periods_listed.txt", sep = '\t',header = TRUE, fileEncoding = "us-ascii", skip=51, nrow=26)
    mirca_crops$CropClass <- gsub(" |\\.|\\/","", mirca_crops$CropClass)  
    
    mapping   <- toolMappingFile(type="cell",name="CountryToCellMapping.csv", readcsv=TRUE)
    cellNames <- mapping$celliso
    names     <- as.vector(outer(outer(outer(mirca_crops$CropClass, c("irrigated","rainfed"), paste, sep="."), c(1:9), paste, sep="."),c("area","start","end"), paste, sep="."))
    mag       <- array(0, dim=c(59199,1,length(names)),dimnames=list(cellNames,NULL,names))
    
    translate_grid2mag <- t(2*t(magpie_coord)+ c(360,180)+0.5)
    
    for(crop in 1:52){
      for (subcrop in 1:9){
        
        grid <- array(0, dim=c(720,360))
        tmp <- subset(grow_p[which(grow_p$crop==crop & grow_p$subcrop==subcrop),], select=c("row","column","area","start","end"))
        irowicol <- cbind(tmp[,"column"],361-tmp[,"row"])
        
        for (i in 1:3){
          grid[irowicol] <- tmp[,i+2]
          mag[,,names[9*52*(i-1)+52*(subcrop-1) + crop]] <- grid[translate_grid2mag]
          
        }
      }
    }
    x <- as.magpie(mag)
    
  } else {stop(paste0("Unknown subtype '",subtype,"'."))}
  

  return(x)
}