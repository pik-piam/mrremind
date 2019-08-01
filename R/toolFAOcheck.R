#' toolFAOcheck
#' 
#' Checks the FAO datasets for known problems. Among other things it compares
#' whether production values from ProdSTAT and Commodity Balances agree,
#' whether values are lost via aggregation.
#' 
#' 
#' @return should return a report
#' @author Ulrich Kreidenweis
#' 
#' @importFrom magclass dimSums
#' 
toolFAOcheck <- function() {
  
  readProd <- function(convert=TRUE, level="country"){
      # read in crop values
    CropPrim <- readSource("FAO", "Crop", convert=convert)
    CropProc <- readSource("FAO", "CropProc", convert=convert)
    
    if (level=="glo") {
      CropPrim[is.na(CropPrim)] <- 0
      CropProc[is.na(CropProc)] <- 0
      CropPrim <- dimSums(CropPrim, dim=1)
      CropProc <- dimSums(CropProc, dim=1)
    }
    
    Crop <- toolFAOcombine(CropPrim,CropProc, combine="Item")
    rm(CropPrim, CropProc)
     
    # read in livestock data
    LivePrim <- readSource("FAO", "LivePrim", convert=convert)
    LiveProc <- readSource("FAO", "LiveProc", convert=convert)
    
    if (level=="glo") {
      LivePrim[is.na(LivePrim)] <- 0
      LiveProc[is.na(LiveProc)] <- 0
      LivePrim <- dimSums(LivePrim, dim=1)
      LiveProc <- dimSums(LiveProc, dim=1)
    }
    
    Live <- toolFAOcombine(LivePrim,LiveProc, combine="Item")
    rm(LivePrim, LiveProc)
  
    Prod <- toolFAOcombine(Crop,Live)
    return(Prod)
  }
  
  readCB <- function(convert=TRUE, level="country") {
    # read in Commodity Balance values
    CBCrop <- readSource("FAO", "CBCrop", convert=convert)
    CBLive <- readSource("FAO", "CBLive", convert=convert)
    
    if (level=="glo") {
      CBCrop[is.na(CBCrop)] <- 0
      CBLive[is.na(CBLive)] <- 0
      CBCrop <- dimSums(CBCrop, dim=1)
      CBLive <- dimSums(CBLive, dim=1)
    }
    
    CB <- toolFAOcombine(CBLive,CBCrop, combine="Item")
    rm(CBCrop,CBLive); gc()
    return(CB)
  }
  
  

  
  ### 1. check: Is data lost though the conversion step?? ###
  
  print("starting first check: is data lost during the conversion?")
  
  ## read in values before conversion
  
  Produncon <- readProd(convert=F, level="glo")
  CBuncon <- readProd(convert=F, level="glo")
  
  ## read in values before conversion
  
  Prod <- readProd(convert=T, level="glo")
  CB <- readProd(convert=T, level="glo")
  
  Produncon[,2000,"production"]/Prod[,2000,"production"]
  
  Produncon[,2000,"production"][,,"Wheat",pmatch=T]/Prod[,2000,"production"][,,"Wheat",pmatch=T]
  
  
  
  ### there seem to be some problems with the conversion!!!
  
  
  # str(Produncon)
  # fulldim(Produncon)
  # fulldim(Prod)
  
  
  ## Problem: unconverted data differs in regions and can not easily be aggregated
  
}







