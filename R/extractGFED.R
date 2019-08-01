#' @title extractGFED
#' @name extractGFED
#' @description 
#' Aggregate GFED grid from 0,25 to 0,50
#' @param datatype Specify which file type has to be read. Only "hdf5" is available at the moment. (Also set as default)
#' @param year Specify which year's data has to be read in from GFED database. Currently only years 1997-2015 available.
#' @param ResChange Specify if the resolution be changed from 0,25 degree cells to 0,5 degree cells. Default is True.
#' @return magpie object of the GFED data
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}} {\link{readGFED}}
#' @examples
#' \dontrun{ a <- readSource(type="12regions_baseline")
#' }
#' @importFrom rhdf5 h5read H5close

extractGFED <- function(datatype="hdf5",year,ResChange=T) {
  start.time <- Sys.time()
  if (datatype=="hdf5") {
    
    # More info on HDF5 file ---- https://support.hdfgroup.org/HDF5/whatishdf5.html
    
    filename <- "GFED4.1s_"
    extension <- paste(".",datatype,sep = "")
    hdf5file <- paste(filename,year,extension,sep = "")
    #Read HDF5 file
    mydata <- h5read(hdf5file,name = "emissions") #Only emissions data are our concern
    # Documentation of GFED emissions data - version 4,1
    # http://www.falw.vu/~gwerf/GFED/GFED4/Readme.pdf
    
    months <- 12
    out <- 0
    
    # ----------------------------------------------------
    
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
    # ----------------------------------------------------
    
    if(ResChange)
    {
    # Now taht the annual data is in hand, Next job is to calculate C and DM emissions for EACH forest type emission.
    # 6 Forest types - 2 indicators each - total 12 calculations each year
    # Note that we are also changing the resolution from 0,25 to 0,50 grids
    # ---CALCULATING "C" EMISSIONS FROM EACH FOREST TYPE---#
    
    # -------------------C_AGRI-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.C_AGRI <- array(0, dim = c(nrow(out[[1]]),ncol(out[[1]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.C_AGRI)){
      for(j in 1:ncol(ColShrink.C_AGRI)){
        ColShrink.C_AGRI[i,j] <- out[[1]][i,(2*j)-1]+out[[1]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.C_AGRI <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.C_AGRI[j,i] <- ColShrink.C_AGRI[((2*j)-1),i]+ColShrink.C_AGRI[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.C_AGRI <- RowCollapse.C_AGRI
    rm(ColShrink.C_AGRI)
    rm(RowCollapse.C_AGRI)
    # ----------------------------------------------------
    
    # -------------------C_BORF-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.C_BORF <- array(0, dim = c(nrow(out[[2]]),ncol(out[[2]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.C_BORF)){
      for(j in 1:ncol(ColShrink.C_BORF)){
        ColShrink.C_BORF[i,j] <- out[[2]][i,(2*j)-1]+out[[2]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.C_BORF <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.C_BORF[j,i] <- ColShrink.C_BORF[((2*j)-1),i]+ColShrink.C_BORF[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.C_BORF <- RowCollapse.C_BORF
    rm(ColShrink.C_BORF)
    rm(RowCollapse.C_BORF)
    # ----------------------------------------------------
    
    # -------------------C_DEFO-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.C_DEFO <- array(0, dim = c(nrow(out[[3]]),ncol(out[[3]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.C_DEFO)){
      for(j in 1:ncol(ColShrink.C_DEFO)){
        ColShrink.C_DEFO[i,j] <- out[[3]][i,(2*j)-1]+out[[3]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.C_DEFO <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.C_DEFO[j,i] <- ColShrink.C_DEFO[((2*j)-1),i]+ColShrink.C_DEFO[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.C_DEFO <- RowCollapse.C_DEFO
    rm(ColShrink.C_DEFO)
    rm(RowCollapse.C_DEFO)
    # ----------------------------------------------------
    
    # -------------------C_PEAT-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.C_PEAT <- array(0, dim = c(nrow(out[[4]]),ncol(out[[4]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.C_PEAT)){
      for(j in 1:ncol(ColShrink.C_PEAT)){
        ColShrink.C_PEAT[i,j] <- out[[4]][i,(2*j)-1]+out[[4]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.C_PEAT <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.C_PEAT[j,i] <- ColShrink.C_PEAT[((2*j)-1),i]+ColShrink.C_PEAT[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.C_PEAT <- RowCollapse.C_PEAT
    rm(ColShrink.C_PEAT)
    rm(RowCollapse.C_PEAT)
    # ----------------------------------------------------
    
    # -------------------C_SAVA-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.C_SAVA <- array(0, dim = c(nrow(out[[5]]),ncol(out[[5]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.C_SAVA)){
      for(j in 1:ncol(ColShrink.C_SAVA)){
        ColShrink.C_SAVA[i,j] <- out[[5]][i,(2*j)-1]+out[[5]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.C_SAVA <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.C_SAVA[j,i] <- ColShrink.C_SAVA[((2*j)-1),i]+ColShrink.C_SAVA[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.C_SAVA <- RowCollapse.C_SAVA
    rm(ColShrink.C_SAVA)
    rm(RowCollapse.C_SAVA)
    # ----------------------------------------------------
    
    # -------------------C_TEMF-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.C_TEMF <- array(0, dim = c(nrow(out[[6]]),ncol(out[[6]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.C_TEMF)){
      for(j in 1:ncol(ColShrink.C_TEMF)){
        ColShrink.C_TEMF[i,j] <- out[[6]][i,(2*j)-1]+out[[6]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.C_TEMF <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.C_TEMF[j,i] <- ColShrink.C_TEMF[((2*j)-1),i]+ColShrink.C_TEMF[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.C_TEMF <- RowCollapse.C_TEMF
    rm(ColShrink.C_TEMF)
    rm(RowCollapse.C_TEMF)
    # ----------------------------------------------------
    
    # ---CALCULATING "DM" EMISSIONS FROM EACH FOREST TYPE---#
    
    # -------------------DM_AGRI-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.DM_AGRI <- array(0, dim = c(nrow(out[[1+6]]),ncol(out[[1+6]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.DM_AGRI)){
      for(j in 1:ncol(ColShrink.DM_AGRI)){
        ColShrink.DM_AGRI[i,j] <- out[[1+6]][i,(2*j)-1]+out[[1+6]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.DM_AGRI <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.DM_AGRI[j,i] <- ColShrink.DM_AGRI[((2*j)-1),i]+ColShrink.DM_AGRI[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.DM_AGRI <- RowCollapse.DM_AGRI
    rm(ColShrink.DM_AGRI)
    rm(RowCollapse.DM_AGRI)
    # ----------------------------------------------------
    
    # -------------------DM_BORF-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.DM_BORF <- array(0, dim = c(nrow(out[[2+6]]),ncol(out[[2+6]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.DM_BORF)){
      for(j in 1:ncol(ColShrink.DM_BORF)){
        ColShrink.DM_BORF[i,j] <- out[[2+6]][i,(2*j)-1]+out[[2+6]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.DM_BORF <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.DM_BORF[j,i] <- ColShrink.DM_BORF[((2*j)-1),i]+ColShrink.DM_BORF[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.DM_BORF <- RowCollapse.DM_BORF
    rm(ColShrink.DM_BORF)
    rm(RowCollapse.DM_BORF)
    # ----------------------------------------------------
    
    # -------------------DM_DEFO-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.DM_DEFO <- array(0, dim = c(nrow(out[[3+6]]),ncol(out[[3+6]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.DM_DEFO)){
      for(j in 1:ncol(ColShrink.DM_DEFO)){
        ColShrink.DM_DEFO[i,j] <- out[[3+6]][i,(2*j)-1]+out[[3+6]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.DM_DEFO <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.DM_DEFO[j,i] <- ColShrink.DM_DEFO[((2*j)-1),i]+ColShrink.DM_DEFO[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.DM_DEFO <- RowCollapse.DM_DEFO
    rm(ColShrink.DM_DEFO)
    rm(RowCollapse.DM_DEFO)
    # ----------------------------------------------------
    
    # -------------------DM_PEAT-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.DM_PEAT <- array(0, dim = c(nrow(out[[4+6]]),ncol(out[[4+6]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.DM_PEAT)){
      for(j in 1:ncol(ColShrink.DM_PEAT)){
        ColShrink.DM_PEAT[i,j] <- out[[4+6]][i,(2*j)-1]+out[[4+6]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.DM_PEAT <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.DM_PEAT[j,i] <- ColShrink.DM_PEAT[((2*j)-1),i]+ColShrink.DM_PEAT[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.DM_PEAT <- RowCollapse.DM_PEAT
    rm(ColShrink.DM_PEAT)
    rm(RowCollapse.DM_PEAT)
    # ----------------------------------------------------
    
    # -------------------DM_SAVA-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.DM_SAVA <- array(0, dim = c(nrow(out[[5+6]]),ncol(out[[5+6]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.DM_SAVA)){
      for(j in 1:ncol(ColShrink.DM_SAVA)){
        ColShrink.DM_SAVA[i,j] <- out[[5+6]][i,(2*j)-1]+out[[5+6]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.DM_SAVA <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.DM_SAVA[j,i] <- ColShrink.DM_SAVA[((2*j)-1),i]+ColShrink.DM_SAVA[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.DM_SAVA <- RowCollapse.DM_SAVA
    rm(ColShrink.DM_SAVA)
    rm(RowCollapse.DM_SAVA)
    # ----------------------------------------------------
    
    # -------------------DM_TEMF-----------------------#
    # Shrink the columns - 0,25*0,50
    ColShrink.DM_TEMF <- array(0, dim = c(nrow(out[[6+6]]),ncol(out[[6+6]])/2)) #It has to still store 1440 rows
    
    for (i in 1:nrow(ColShrink.DM_TEMF)){
      for(j in 1:ncol(ColShrink.DM_TEMF)){
        ColShrink.DM_TEMF[i,j] <- out[[6+6]][i,(2*j)-1]+out[[6+6]][i,2*j]
      }
    }
    
    # ----------------------------------------------------
    
    #Collpase the rows: New Resolution = 0,50*0,50
    RowCollapse.DM_TEMF <- array(0, dim = c(720,360)) #It has to now store 720 rows
    for (i in 1:360){
      for(j in 1:720){
        RowCollapse.DM_TEMF[j,i] <- ColShrink.DM_TEMF[((2*j)-1),i]+ColShrink.DM_TEMF[(2*j),i]
      }
    }
    
    # ----------------------------------------------------
    halfRes.DM_TEMF <- RowCollapse.DM_TEMF
    rm(ColShrink.DM_TEMF)
    rm(RowCollapse.DM_TEMF)
    rm(out)
    # ----------------------------------------------------
    
    Emissions <-list(halfRes.C_AGRI,halfRes.C_BORF,halfRes.C_DEFO,halfRes.C_PEAT,halfRes.C_SAVA,halfRes.C_TEMF,
                           halfRes.DM_AGRI,halfRes.DM_BORF,halfRes.DM_DEFO,halfRes.DM_PEAT,halfRes.DM_SAVA,halfRes.DM_TEMF)
    names(Emissions)<-names(month[[1]]) #Getting back the names
    rm(month)
    end.time <- Sys.time()
    time.taken <- paste("Step completed in ",round(end.time - start.time,digits = 1)," seconds.")
    vcat(verbosity = 1, paste("Resolution change was requested. Returning emissions data for the year",year, "\nSource: GFED Database\nResolution : 0,50 degree grid","\n",time.taken))
    return(Emissions)
    }
    if(!ResChange){
      end.time <- Sys.time()
      time.taken <- paste("Step completed in ",round(end.time - start.time,digits = 1)," seconds.")
      vcat(verbosity = 1, paste("Resolution change wasn't requested. Returning emissions data for the year ",year, " from GFED Database on 0,25 degree grid", time.taken))
      return(out)
    }
    H5close()
    }

  else (stop("Invalid filetype selected: Only hdf5 available at the moment"))
  end.time <- Sys.time()
  time.taken <- paste("Step completed in ",round(end.time - start.time,digits = 1)," seconds.")
  vcat(verbosity = 1, time.taken)
}  

