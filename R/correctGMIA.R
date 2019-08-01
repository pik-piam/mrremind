#'@title correctGMIA
#' @description Correct Irrigated Area
#' 
#' Correct Irrigated Area to 0.5 Degree x 0.5 Degree Grid.
#' Change resolution from 5 arcmin to 0.5 Degree by aggregating. Values in ha are summed up, Values in percent are calculated using mean.
#' 
#' @param x MAgPIE object containing Global Map on Irrigaiton data data at 0.5 Degree resolution
#' @param subtype : subtypes are the same as in readGMIA
#' @return Global Map on Irrigation data as MAgPIE object at a 0.5 Degree resolution.
#' @author Stephen Wirth
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{
#' a <- readSource("IrrigatedArea")
#' }
#' @importFrom raster raster extent<- as.matrix aggregate
#' 
#' 

correctGMIA <- function(x, subtype){
  files=c(all_data_national="HESS_2010_159_Supplement_S2.csv",
          aei_pct = "gmia_v5_aei_pct.asc",
          aei_ha = "gmia_v5_aei_ha.asc",
          aai_pct_aei = "gmia_v5_aai_pct_aei.asc",
          aeigw_pct_aei = "gmia_v5_aeigw_pct_aei.asc",
          aeisw_pct_aei = "gmia_v5_aeisw_pct_aei.asc",
          aeinc_pct_aei = "gmia_v5_aeinc_pct_aei.asc")
  
  file <- toolSubtypeSelect(subtype, files)
  if(subtype=="all_data_national"){
    return(x)
  }
  else{
  #getCelnames and extent
    mapping <- toolMappingFile(type="cell", readcsv=T, name="CountryToCellMapping.csv")
  cellNames <- mapping[,1]
  lon <- seq(-179.75,179.75,by=0.5)
  lat <- rev(seq(-89.75,89.75,by=0.5))
  
  #read file
  x <- raster(file)
  extent(x) <- c(-180,180,-90,90)
  #checkk for subtype and aggregate according to it
  if(grepl("ha", subtype))
  {
  x <- aggregate(x,fact=6,fun=sum)
  }
  else
  {
    x <- aggregate(x,fact=6,fun=mean)
  }
  #raster to matrix to magpie
  x <- t(raster::as.matrix(x))
  mag <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,NULL,NULL))
  vars <- c("lon","lat")
  coord <- mapping[vars]
  coord$lon <- as.numeric(coord$lon)
  coord$lat <- as.numeric(coord$lat)
  #Optimization? mag <- x[coord[,1],coord[,2]]
  for (j in 1:59199) {
    mag[j,,] <- x[which(coord[j, 1]==lon), which(coord[j,2]==lat)]
  }
  
  y <- as.magpie(mag,spatial=1,temporal=2)
  z <- new.magpie(cells_and_regions = getCells(y),years = 2000:2008,names = subtype,sets = getSets(y))
  z[,2000:2008,] <- y
  

return(z)
  }
}