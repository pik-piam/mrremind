#' Convert FeedModel data
#' 
#' Convert production system distribution and feed basket data to ISO country level.
#' 
#' 
#' @param x MAgPIE object containing production system distribution and feed basket data.
#' @param subtype Available subtypes: "ProdSysRatio", "FeedBaskets" and "FeedBasketsDetailed"
#' @return Production system distribution and feed basket data as MAgPIE object on ISO country level
#' @author Isabelle Weindl, Jan Philipp Dietrich
#' @importFrom utils tail

convertFeedModel <- function(x,subtype="FeedBaskets") {
  
  timesteps<-getYears(x)
  year <- tail(timesteps,1)
  
  createNAmatrix <- function(x) {
    tmp     <- x
    tmp[!x] <- 0
    tmp[x]  <- NA
    return(tmp)
  }
  
  # identify missing entries and fill with country mean or global mean
  fillmissing <- function(x, datadim=3.2, glo=TRUE) {
    if(glo) {
      dim <- c(2,datadim) ; dim2 <- 3
    } else {
      dim <- datadim; dim2 <- c(1,3)
    }
    missing <- (dimSums(x,dim=dim)==0)
    val_mean <- as.magpie(apply(x + createNAmatrix(missing),dim2,mean, na.rm=TRUE))
    val_mean[is.nan(val_mean)] <- 0
    return(x + missing*val_mean)
  }

  if(subtype=="ProdSysRatio"){
    x <- fillmissing(x, datadim=3, glo=FALSE)
    x <- fillmissing(x, datadim=3, glo=TRUE)
    
    #some cases are not handled by the above executed automated filling of missing values
    #these are tackled by following replacement rules:
    missing_all <- where(x[,tail(timesteps,3),"sys_beef.livst_rum"]<0.08)$true$regions
    replacement <- as.magpie(apply(x[missing_all,tail(timesteps,3),"sys_beef.livst_rum"],c(1,3),mean, na.rm=TRUE))
    for(t in tail(timesteps,3)){
      missing_t <- where(x[,t,"sys_beef.livst_rum"]<0.08)$true$regions
      x[missing_t,t,"sys_beef.livst_rum"] <- setYears(replacement[missing_t,,],t)
    }
    
    #re-establishment of the underlying properties of the data set:
    x[,,"sys_dairy.livst_milk"] <- 1
    x[,,"sys_dairy.livst_rum"] <- 1 - x[,,"sys_beef.livst_rum"]
    x[,,"sys_pig.livst_pig"] <- 1
    x[,,"sys_hen.livst_egg"] <- 1
    x[,,"sys_hen.livst_chick"] <- 1 - x[,,"sys_chicken.livst_chick"]
    
  }else{
    x <- fillmissing(x, datadim=3.2, glo=FALSE)
    x <- fillmissing(x, datadim=3.2, glo=TRUE)
  }
  
  return(x)
}

