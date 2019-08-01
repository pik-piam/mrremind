#' @importFrom ncdf4 nc_open
#' @importFrom raster raster extent brick subset aggregate projectRaster
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %:% %dopar%
#' @importFrom abind abind
#' @importFrom magclass as.magpie mbind

#  %:% %dopar% abind brick data_sel detectCores foreach getCoordinates
#makeCluster out outfile path registerDoParallel stopCluster time_sel

readLUH2v2 <- function(subtype) {
  
  if(subtype=="states") {
    
    #File to process
    f <- "states.nc"
    
    #select years
    start_year <- 1900 #min 850
    end_year <- 2015 #max 2015
    timesteps <- 1
    offset <- 849 #year 850=1, year 1900=1051, year 2015=1166
    
    #define dims
    mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE) 
     
    cellNames <- mapping$celliso
    lon <- seq(-179.75,179.75,by=0.5)
    lat <- rev(seq(-89.75,89.75,by=0.5))
    time <- paste0("y",seq(start_year,end_year,by=timesteps)) 
    nc_file <- nc_open(f)
    data <- setdiff(names(nc_file$var),c("secma","secmb","lat_bounds","lon_bounds"))#[1:2]
    
    #land area
    carea <- raster("staticData_quarterdeg.nc",varname="carea")
    extent(carea) <- c(-180,180,-90,90)
    
    #parallel setup
 #   if(getConfig("parallel")){
    no_cores <- getConfig("nocores")
    cl <- makeCluster(no_cores,outfile="par_debug.txt")
    registerDoParallel(cl)
    
    #Do the stuff
    acomb2 <- function(...) abind(..., along=2)
    acomb3 <- function(...) abind(..., along=3)
    #.maxcombine=10
    data_sel <- time_sel <- NULL
    x <- foreach(data_sel=1:length(data),.combine='acomb3',.multicombine=TRUE, .export=c("start_year","end_year","timesteps","magpie_coord","cellNames","brick","subset","aggregate","abind")) %:%
      foreach(time_sel=seq(start_year-offset,end_year-offset,by=timesteps),.combine='acomb2',.multicombine=TRUE) %dopar% {
        print(data[data_sel])
        print(time_sel+offset)
        shr <- brick(f,varname=data[data_sel])
        shr <- subset(shr,time_sel)
        x <- shr*carea
        x <- aggregate(x,fact=2,fun=sum)
        x <- t(as.matrix(x))
        mag <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,paste0("y",time_sel+offset),data[data_sel]))
        for (j in 1:59199) {
          mag[j,,] <- x[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
        }
        return(mag)
      }
    stopCluster(cl)
    
    gc()
    # }  else  {
    #   mag <- data_sel <- time_sel <- NULL
    #  #TODO: create x in the correct dimensions then run loop and add things to the corresponding entries.
    #  x <- array(NA, dim = c(59199, length(seq(start_year-offset,end_year-offset,by=timesteps)), length(data)),
    #             dimnames=list(cellNames, paste0("y", seq(start_year-offset,end_year-offset,by=timesteps)+offset), data))
    #   for(data_sel in 1:length(data)){
    #     for(time_sel in seq(start_year-offset,end_year-offset,by=timesteps)){
    #       print(data[data_sel])
    #       print(time_sel+offset)
    #       shr <- brick(f,varname=data[data_sel])
    #       shr <- subset(shr,time_sel)
    #       xi <- shr*carea
    #       xi <- aggregate(xi,fact=2,fun=sum)
    #       xi <- t(as.matrix(xi))
    #       mag <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,paste0("y",time_sel+offset),data[data_sel]))
    #       for (j in 1:59199) {
    #         #mag[j,,] <- xi[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
    #         x[j,paste0("y",time_sel+offset),data_sel] <- xi[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
    #       }
    #     }
    #   #  x <- mbind(as.magpie(x), as.magpie(xt))
    #   }
    #   
    #   gc()
    # }
    x <- as.magpie(x,spatial=1,temporal=2)
    
    #convert from km^2 to Mha
    x <- x/10000
    
    #remove NAs
    x[is.na(x)] <- 0
    
  } else if (subtype=="irrigation") {
    #File to process
    f_states <- "states.nc"
    f_man <- "management.nc"
    
    #select years
    start_year <- 1900 #min 850
    end_year <- 2015 #max 2015
    timesteps <- 1
    offset <- 849 #year 850=1, year 1900=1051, year 2015=1166
    
    #define dims
    mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE) 
    cellNames <- mapping$celliso
    lon <- seq(-179.75,179.75,by=0.5)
    lat <- rev(seq(-89.75,89.75,by=0.5))
    time <- paste0("y",seq(start_year,end_year,by=timesteps)) 
    # nc_file <- nc_open(f_man)
    # data <- setdiff(names(nc_file$var),c("secma","secmb","lat_bounds","lon_bounds"))#[1:2]
    #Mapping between states and management_irrigation 
    data_man <- c("irrig_c3ann","irrig_c3per","irrig_c4ann","irrig_c4per","irrig_c3nfx","flood")
    data_states <- c("c3ann","c3per","c4ann","c4per","c3nfx","c3ann")
    data <- matrix(data=c(data_man,data_states),ncol=2)
    
    #get coords
    data("magpie_coord", envir=environment(), package = "moinput")
    
    #land area
    carea <- raster("staticData_quarterdeg.nc",varname="carea")
    extent(carea) <- c(-180,180,-90,90)
    
    #parallel setup
   # if(getConfig("parallel")){
      no_cores <-  getConfig("nocores")
#    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores,outfile="par_debug.txt")
    registerDoParallel(cl)
    
    #Do the stuff
    acomb2 <- function(...) abind(..., along=2)
    acomb3 <- function(...) abind(..., along=3)
    #.maxcombine=10
    x <- foreach(data_sel=1:length(data_man),.combine='acomb3',.multicombine=TRUE, .export=c("start_year","end_year","timesteps","magpie_coord","cellNames","brick","subset","aggregate","abind")) %:%
      foreach(time_sel=seq(start_year-offset,end_year-offset,by=timesteps),.combine='acomb2',.multicombine=TRUE) %dopar% {
        print(data[data_sel,1])
        print(time_sel+offset)
        shr <- brick(f_states,varname=data[data_sel,2])
        shr <- subset(shr,time_sel)
        ir_shr <- brick(f_man,varname=data[data_sel,1])
        ir_shr <- subset(ir_shr,time_sel)
        #grid cell fraction of crop area x grid cell area x irrigated fraction of crop area
        x <- shr*carea*ir_shr
        x <- aggregate(x,fact=2,fun=sum)
        x <- t(as.matrix(x))
        mag <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,paste0("y",time_sel+offset),data[data_sel,1]))
        for (j in 1:59199) {
          mag[j,,] <- x[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
        }
        return(mag)
      }
    stopCluster(cl)

    gc()
    # }else{
    #   mag <- data_sel <- time_sel <- NULL
    #   #TODO: create x in the correct dimensions then run loop and add things to the corresponding entries.
    #   x <- array(NA, dim = c(59199, length(seq(start_year-offset,end_year-offset,by=timesteps)), length(data[,1])),
    #              dimnames=list(cellNames, paste0("y", seq(start_year-offset,end_year-offset,by=timesteps)+offset), data[,1]))
    #   for(data_sel in 1:length(data_man)){
    #     for(time_sel in seq(start_year-offset,end_year-offset,by=timesteps)){
    #       print(data[data_sel,1])
    #       print(time_sel+offset)
    #       shr <- brick(f_states,varname=data[data_sel,2])
    #       shr <- subset(shr,time_sel)
    #       ir_shr <- brick(f_man,varname=data[data_sel,1])
    #       ir_shr <- subset(ir_shr,time_sel)
    #       #grid cell fraction of crop area x grid cell area x irrigated fraction of crop area
    #       xi <- shr*carea*ir_shr
    #       xi <- aggregate(xi,fact=2,fun=sum)
    #       xi <- t(as.matrix(xi))
    #       mag <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,paste0("y",time_sel+offset),data[data_sel,1]))
    #       for (j in 1:59199) {
    #       #  mag[j,,] <- xi[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
    #         x[j,paste0("y",time_sel+offset),data_sel] <- xi[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
    #       }
    #     }
    #     #x <- mbind(as.magpie(x), as.magpie(mag))
    #   } 
    #   gc()
   # }
    
    x <- as.magpie(x,spatial=1,temporal=2)
    
    #convert from km^2 to Mha
    x <- x/10000
    
    #remove NAs
    x[is.na(x)] <- 0
    
    x<-clean_magpie(x)
    
  } else if (subtype=="ccode") {
    
    #Configure longitude and latitude on 0.5°
    lon       <- seq(-179.75,179.75,by=0.5)
    lat       <- rev(seq(-89.75,89.75,by=0.5))
    
    #Load celliso names for 1:59199 magpie cells
    mapping   <- toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE) 
    cellNames <- mapping$celliso
    
    #Load raster data on 0.25° and extend to full grid
    ccode25         <- raster("staticData_quarterdeg.nc",varname="ccode")
    extent(ccode25) <- c(-180,180,-90,90)
    
    #Create new raster object on 0.5° and re-project 0.25°-raster on 0.5°-raster
    r50     <- raster(res=0.5)
    ccode50 <- projectRaster(ccode25,r50,over=TRUE, method="ngb") #re-project to regular grid
    
    #Change longitude and latitude 
    ccode50 <- t(as.matrix(ccode50))
    
    #Create array for 59199 isocells, 1 year and 1 data dimension
    mag   <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,"y2000","ccode"))
    
    #Fill array with data from raster object (note magpie_coord are loaded by default)
    for (j in 1:59199) {
      mag[j,,] <- ccode50[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
    }
    
    #Convert array to magpie object and rename set
    x          <- clean_magpie(as.magpie(mag))
    getSets(x) <- c("cell","t","ccode")

  }
  
  return(x)
}
  
