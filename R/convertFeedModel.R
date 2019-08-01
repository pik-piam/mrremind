#' Convert FeedModel data
#' 
#' Convert production system distribution and feed basket data to ISO country level.
#' 
#' 
#' @param x MAgPIE object containing production system distribution and feed basket data.
#' @return Production system distribution and feed basket data as MAgPIE object on ISO country level
#' @author Isabelle Weindl, Jan Philipp Dietrich

convertFeedModel <- function(x) {
  
  createNAmatrix <- function(x) {
    tmp     <- x
    tmp[!x] <- 0
    tmp[x]  <- NA
    return(tmp)
  }
  
  # identify missing entries and fill with country mean or global mean
  fillmissing <- function(x, glo=TRUE) {
    if(glo) {
      dim <- c(2,3.2) ; dim2 <- 3
    } else {
      dim <- 3.2; dim2 <- c(1,3)
    }
    missing <- (dimSums(x,dim=dim)==0)
    val_mean <- as.magpie(apply(x + createNAmatrix(missing),dim2,mean, na.rm=TRUE))
    val_mean[is.nan(val_mean)] <- 0
    return(x + missing*val_mean)
  }
  
  x <- fillmissing(x, glo=FALSE)
  x <- fillmissing(x, glo=TRUE)
  return(x)
}

