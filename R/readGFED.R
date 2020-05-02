#' Read GFED
#' 
#' Read-in an GFED file as magclass object
#' 
#' 
#' @param subtype Currently available "12regions_baseline", "emissions" and "emissionfactors". "emissionfactors" reads the different chemical ploutants provided in the GFED data.  
#' @return magpie object of the GFED data
#' @author Abhijeet Mishra, Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("GFED","emissions")
#' }
#' 
#' @import ncdf4
#' @import rhdf5
#' @import madrat
#' @importFrom magclass clean_magpie ncells

readGFED <- function(subtype) {
  
  if (subtype=="12regions_baseline"){
    #file <- "gfed.csv"
    out2 <- NULL
    load("gfed.rda")
    if(is.null(out2)) stop("out2 does not exist in gfed.rda!")
    return(out2)
  } else if (subtype=="emissionfactors") {
    data <- read.table("GFED4_Emission_Factors.txt")
    colnames(data) <- c("SPECIE","SAVA","BORF","TEMF","DEFO","PEAT","AGRI")
    levels(data$SPECIE) <- sub("2.5", "2_5", levels(data$SPECIE))
    rownames(data)<-data$SPECIE
    data<-data[,-1]
    x <- data.matrix(data,rownames.force = T)
    xt <- t(x) #Transposing x to make sure that we get forestype.indicator
    x <- as.magpie(xt)
    x <- clean_magpie(x)
    return(x)
  } else if (subtype=="emissions"){
    start <- 1997
    finish <- 2018
    
    ## Read as MAgPIE object
    all<-NULL
    for(year in start:finish){
      vcat(verbosity = 1,paste0("Reading data for y",year))
      # start_time <- Sys.time()
      mydata <- h5read(paste0("GFED4.1s_",start,".hdf5"),name = "emissions")
      # Documentation of GFED emissions data - version 4,1
      # http://www.falw.vu/~gwerf/GFED/GFED4/Readme.pdf
      
      months <- 12
      out <- 0
      
      for (i in 1:months){
        month <- mydata[[i]][c("C","DM","partitioning")] #We require only c, DM and Partition data. Rest all is not subsetted
        month$TotalEmissions <- month$partitioning #Just to grab the names
        # Idea is to  multiply C and Dm contents by their partition factors 
        for(j in 1:(length(month$partitioning)/2)){ 
          # First 6 (i.e. 12/2) elements inside "partitioning" correspond to C emission partitions 
          # from 6 different sources
          month$TotalEmissions[[j]] <- month$partitioning[[j]]*month$C
        }
        for(k in (1+length(month$partitioning)/2):length(month$partitioning)){ 
          # Next 6 (i.e. 1+(12/2) to 12) elements inside "partitioning" correspond to DM emission partitions 
          # from 6 different sources
          month$TotalEmissions[[k]] <- month$partitioning[[k]]*month$C
        }
        # Now that we have disaggregated emissions of 6 differen types for C and DM emissions
        # So we don't need C,DM and Partitiom   
        month <- month[-c(1,2,3)] 
        out <- Map("+",out,month[[1]]) #Keep adding for annual data - each month is added to preeceeding sum
        names(out)<-names(month[[1]]) #Getting back the names
      }
      rm(mydata)
      
      emi_names <- names(out)
      halfRes <- list()
      for(n in emi_names){
        # Shrink the columns: New Resolution = 0,25*0,50
        ColShrink <- array(0, dim = c(nrow(out[[n]]),ncol(out[[n]])/2)) #It has to still store 1440 rows
        for (i in 1:nrow(ColShrink)){
          for(j in 1:ncol(ColShrink)){
            ColShrink[i,j] <- out[[n]][i,(2*j)-1]+out[[n]][i,2*j]
          }
        }
        #Collpase the rows: New Resolution = 0,50*0,50
        RowCollapse <- array(0, dim = c(720,360)) #It has to now store 720 rows
        for (i in 1:360){
          for(j in 1:720){
            RowCollapse[j,i] <- ColShrink[((2*j)-1),i]+ColShrink[(2*j),i]
          }
        }
        halfRes[[n]] <- RowCollapse
        rm(ColShrink)
        rm(RowCollapse)
      }
      
      ######################################################
      
      lon3 <- as.array(seq(-179.750,179.750,0.50))
      lat3 <- as.array(seq(89.750,-89.750,-0.50))
      
      # define dimensions
      londim <- ncdim_def("lon","degrees_east",as.double(lon3)) 
      latdim <- ncdim_def("lat","degrees_north",as.double(lat3))
      
      # define variables
      fillvalue <- 1e32
      var_def <- list()
      for(n in emi_names){
        dlname <- n
        if(length(grep("C_",n))!=0){
          tmp_def <- ncvar_def(n,"gC/m2",list(londim,latdim),fillvalue,dlname,prec="single")  
        }else{
          tmp_def <- ncvar_def(n,"KgDM/m2",list(londim,latdim),fillvalue,dlname,prec="single")
        }
        var_def[[n]] <- tmp_def 
      }
      
      # create netCDF file and put arrays
      ncfname <- paste0("GFED_",year,".nc")
      
      ncout <- nc_create(ncfname,var_def,force_v4=T)  
      
      # put variables
      for(n in emi_names){
        ncvar_put(ncout,var_def[[n]],halfRes[[n]])
      }
      
      # put additional attributes into dimension and data variables
      ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
      ncatt_put(ncout,"lat","axis","Y")
      
      # add global attributes
      ncatt_put(ncout,0,"Title","GFED file converted from hdf5 file")
      ncatt_put(ncout,0,"Institution","PIK")
      ncatt_put(ncout,0,"Source","https://www.geo.vu.nl/~gwerf/GFED/GFED4/")
      history <- paste("Abhijeet Mishra", date(), sep=", ")
      ncatt_put(ncout,0,"History",history)
      
      if (is.null(ncout$dim$time$len)) 
        ncout$dim$time$len <- 1
      if (is.null(ncout$dim$time$vals)) 
        ncout$dim$time$vals <- year
      nc_data <- array(NA, dim = c(ncout$dim$lon$len,
                                   ncout$dim$lat$len, 
                                   ncout$dim$time$len, 
                                   ncout$nvars))
      for (i in 1:ncout$nvars) {
        nc_data[, , , i] <- ncvar_get(ncout, names(ncout$var)[i])
      }
      lat <- ncout$dim$lat$vals
      lon <- ncout$dim$lon$vals
      mapping <- toolMappingFile(type = "cell",name = "CountryToCellMapping.csv",readcsv = T)
      coord <- mapping[,c("lon", "lat")]
      mag <- array(NA, dim = c(59199, ncout$dim$time$len,ncout$nvars), 
                   dimnames = list(mapping$celliso, 
                                   paste("y", ncout$dim$time$vals,sep = ""), 
                                   names(ncout$var)))
      # nc_close(ncout)
      for (i in 1:ncells(mag)) {
        mag[i, , ] <- nc_data[which(coord[i, 1] == lon), 
                              which(coord[i, 2] == lat), , ]
      }
      x.magpie <- as.magpie(mag)
      
      all<-mbind(all,x.magpie)
      # fin_time <- Sys.time()
      # vcat(verbosity = 1,paste0("Finished in: ",round(fin_time-start_time,2)," seconds"))
    }
    
    nc_close(ncout)
    unlink(ncfname, recursive = FALSE)
    
    return(all) #Return back the consolidated MO to function run
  }else (stop("subtype does not exist"))

}  
