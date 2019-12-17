#' @title readGridPop
#' @description Reads in past and future (SSP1-5) gridded population data, from ISIMIP database, Past data is based on HYDE3.2, while future SSPs are based on projections from Jones & O'Neill 2016 
#' @return A MAgPIE object, cellular 0.5deg resolution, of population (millions)
#' @param subtype past (1965-2005) or future (2010-2100)
#' @importFrom raster brick subset aggregate 
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %:% %dopar%
#' @importFrom abind abind
#' @importFrom magclass as.magpie mbind


readGridPop <- function(subtype) {

 if (subtype=="past"){
    b <- brick("population_histsoc_2.5min_annual_1861-2005.nc4")

    #select years
    start_year <- 1965 #min 1861
    end_year <- 2005 #max 2015
    timesteps <- 1
    offset=1860
    #year 850=1, year 1900=1051, year 2015=1166 timestep min=1
    
    mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE) 
    cellNames <- mapping$celliso
    lon <- seq(-179.75,179.75,by=0.5)
    lat <- rev(seq(-89.75,89.75,by=0.5))
    time <- paste0("y",seq(start_year,end_year,by=timesteps)) 
    data <- "number_of_people"
    
    
    no_cores <- getConfig("nocores")
    cl <- makeCluster(no_cores,outfile="par_debug.txt")
    registerDoParallel(cl)
    
    #Do the stuff
    acomb2 <- function(...) abind(..., along=2)
    #.maxcombine=10
    data_sel <- time_sel <- NULL
    
    x <- foreach(time_sel=seq(start_year-offset,end_year-offset,by=timesteps),.combine='acomb2',.multicombine=TRUE, .export=c("start_year","end_year","timesteps","magpie_coord","cellNames","brick","subset","aggregate","abind")) %dopar% {
      print(time_sel+offset)
      slice <- subset(b,time_sel)
      slice <- aggregate(slice,fact=12,fun=sum)
      toMag <- t(as.matrix(slice))
      mag <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,paste0("y",time_sel+offset),"number_of_people"))
      for (j in 1:59199) {
        mag[j,,] <- toMag[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
      }
      return(mag)
    }
    
    stopCluster(cl)
    gc()
    
    x <- as.magpie(x,spatial=1,temporal=2)}
  
  if(subtype=="future"){
    
    files <- c("population_ssp1soc_0p5deg_annual_2006-2100.nc4",
               "population_ssp2soc_0p5deg_annual_2006-2100.nc4",
               "population_ssp3soc_0p5deg_annual_2006-2100.nc4",
               "population_ssp4soc_0p5deg_annual_2006-2100.nc4",
               "population_ssp5soc_0p5deg_annual_2006-2100.nc4")
    
    read <- function(file){
      b <- brick(file)
      #select years
      start_year <- 2006 #min 2006
      end_year <- 2100 #max 2100
      timesteps <- 1
      offset=2005
      #year 850=1, year 1900=1051, year 2015=1166
      
      mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE) 
      cellNames <- mapping$celliso
      lon <- seq(-179.75,179.75,by=0.5)
      lat <- rev(seq(-89.75,89.75,by=0.5))
      time <- paste0("y",seq(start_year,end_year,by=timesteps)) 
      data <- "number_of_people"
      
      no_cores <- getConfig("nocores")
      cl <- makeCluster(no_cores,outfile="par_debug.txt")
      registerDoParallel(cl)
      
      #Do the stuff
      acomb2 <- function(...) abind(..., along=2)
      #.maxcombine=10
      data_sel <- time_sel <- NULL
      
      y <- foreach(time_sel=seq(start_year-offset,end_year-offset,by=timesteps),.combine='acomb2',.multicombine=TRUE, .export=c("start_year","end_year","timesteps","magpie_coord","cellNames","brick","subset","aggregate","abind")) %dopar% {
        print(time_sel+offset)
        slice <- subset(b,time_sel)
        toMag <- t(as.matrix(slice))
        mag <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,paste0("y",time_sel+offset),"number_of_people"))
        for (j in 1:59199) {
          mag[j,,] <- toMag[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
        }
        return(mag)
      }
      
      stopCluster(cl)
      gc()
      
      y <- as.magpie(y,spatial=1,temporal=2)
      
      return(y)
    }
    
    a1 <-read(files[1])
    a2 <- read(files[2])
    a3 <- read(files[3])
    a4 <- read(files[4])
    a5 <- read(files[5])
    
    x <- mbind(a1,a2,a3,a4,a5)
    getNames(x) <- c("pop_SSP1","pop_SSP2","pop_SSP3","pop_SSP4","pop_SSP5")
    
  }

return(x)
}
