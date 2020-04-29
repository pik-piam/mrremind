#' @title readUrbanLandFuture
#' @description read in gridded future urban land use datasets
#' @param source Currently only from LUH2v2 for all SSPs, LUH2v2 uses IMAGE
#' @return magpie object of gridded future urban land use in Mha, 2015-2100
#' @author David Chen
#' @seealso \code{\link{readSource}}
#' @importFrom magclass as.magpie 
#' @importFrom ncdf4 nc_open
#' @importFrom raster raster extent brick subset aggregate projectRaster extent<-
#' @importFrom magclass as.magpie mbind


readUrbanLandFuture <- function(subtype) {
if(subtype=="LUH2v2"){
  files <- list.files(path=paste0("./"),  pattern='states', all.files=TRUE, full.names=FALSE)
  
  #select years
  start_year <- 2015 #min 850
  end_year <- 2100 #max 2015
  timesteps <- 1
  offset <- 2015 #year 2016=1, y2100=86
  
  #define dims
  mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)
  cellNames <- mapping$celliso
  lon <- seq(-179.75,179.75,by=0.5)
  lat <- rev(seq(-89.75,89.75,by=0.5))
  time <- paste0("y",seq(start_year,end_year,by=timesteps))
 # nc_file <- nc_open(files[1])
 # data <- setdiff(names(nc_file$var),c("secma","secmb","lat_bounds","lon_bounds"))#[1:2]
  
  #land area
  carea <- raster("staticData_quarterdeg.nc",varname="carea")
  extent(carea) <- c(-180,180,-90,90)
  
  data_sel <- "urban" #can change if ever other future LUH2v2 land use types are needed
  time_sel = seq(start_year-offset,end_year-offset,by=timesteps)
  mag <- array(NA,dim=c(59199,86,5),dimnames=list(cellNames,paste0("y",time_sel+offset),files))

  for (i in 1:length(files)){
  for (t in c(1:length(time_sel))){
     #print(files[i])   
     #print(t)
     shr <- brick(files[i],varname=data_sel)
     shr <- subset(shr,t)
     x <- shr*carea
     x <- aggregate(x,fact=2,fun=sum)
     x <- t(as.matrix(x))
     for (j in 1:59199) {
       mag[j,t,i] <- x[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
     }
  }}

    x <- as.magpie(mag,spatial=1,temporal=2)
    x <- collapseNames(x)

    #convert from km^2 to Mha
    x <- x/10000
    
    #remove NAs
    x[is.na(x)] <- 0
    getNames(x) <- paste0("SSP",c(1:5))
    return(x)

}
  if(subtype!="LUH2v2") {
    stop("Not a Valid Subtype")
  }
}
