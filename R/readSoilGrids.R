#' @title readSoilGrids
#' @description This function reads the raw SoilGrids data (available at https://files.isric.org/soilgrids/data/recent/OCSTHA_M_30cm_250m_ll.tif)
#' or if available the preprocessed raster layers.
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#' @param subtype  Switch between different input. Use predefined ones or any FileName specified in 'SoilGrids/META_GEOTIFF_1B.csv'
#'
#' @seealso
#' \code{\link{downloadSoilGrids}}
#'
#' @examples
#' \dontrun{ readSource("SoilGrids", subtype = "cstock_0_30") }
#'
#' @import madrat
#' @import magclass
#' @importFrom raster raster aggregate res projectRaster writeRaster as.matrix

readSoilGrids <- function(subtype) {

  if(grepl(".tif",subtype)){
    file  <- subtype
  } else{

    files <- c(cstock_0_30  = "OCSTHA_M_30cm_250m_ll.tif", # Soil organic carbon stock in tons per ha for depth interval 0.00 m - 0.30 m
               sandfrac_0   = "SNDPPT_M_sl1_250m_ll.tif",  # Sand content (50-2000 micro meter) mass fraction in % at depth 0.00 m
               sandfrac_5   = "SNDPPT_M_sl2_250m_ll.tif",	 # Sand content (50-2000 micro meter) mass fraction in % at depth 0.05 m
               sandfrac_15  = "SNDPPT_M_sl3_250m_ll.tif",	 # Sand content (50-2000 micro meter) mass fraction in % at depth 0.15 m
               sandfrac_30  = "SNDPPT_M_sl4_250m_ll.tif")	 # Sand content (50-2000 micro meter) mass fraction in % at depth 0.30 m
    file  <- toolSubtypeSelect(subtype,files)
  }

  if(!file.exists(paste0(subtype,".grd"))){

    tmp     <- raster(file)
    out     <- aggregate(tmp, fact=240, fun=mean)
    writeRaster(out, filename=paste0(subtype,".grd"))

  } else {

    out <- raster(paste0(subtype,".grd"))

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
  mag   <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,"y2000",subtype))

  #Fill array with data from raster object (note magpie_coord are loaded by default)
  for (j in 1:59199) {
    mag[j,,] <- toMag[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
  }

  #Convert array to magpie object and rename set
  x          <- clean_magpie(as.magpie(mag))
  getSets(x) <- c("cell","t","data")

  return(x)
}

