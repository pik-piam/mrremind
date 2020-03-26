#' toolHarmonize2Baseline
#'
#' @param x magclass object that should be set on baseline
#' @param base magclass object for baseline
#' @param ref_year Reference year
#' @param limited if TRUE, for an underestimated baseline the signal is limited to the additive term rather than the multiplicative factor
#'                if FALSE, x will be put on the baseline as a pure multiplicative factor
#' @param hard_cut Switch to TRUE for data that can not be harmonized, but have to be glued together
#'
#' @return the averaged data in magclass format
#' @author Kristine Karstens
#'
#' @export

toolHarmonize2Baseline <- function(x, base, ref_year, limited=TRUE, hard_cut=FALSE){
  
  if(!is.magpie(x) | !is.magpie(base)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  
  # check for negative range of values
  negative <- (any(x<0) | any(base<0))
  
  # check if years are overlapping and refs is part of both time horizons
  if(!ref_year%in%intersect(getYears(x),getYears(base))) stop("Overlapping time period of baseline and data is not including the reference year!")
  
  # set years
  years       <- union(getYears(x), getYears(base))
  till_ref    <- getYears(base, as.integer=TRUE)
  till_ref    <- paste0("y",till_ref[till_ref <= as.numeric(substring(ref_year,2))])
  after_ref   <- getYears(x, as.integer=TRUE)
  after_ref   <- paste0("y",after_ref[after_ref > as.numeric(substring(ref_year,2))])
  
  
  #check if x and base are identical in dimension except time
  if(!setequal(getCells(x),getCells(base)) | !setequal(getNames(x),getNames(base))) stop("Dimensions of the MAgPIE objects do not match!")
  
  # create new magpie object with full time horizon
  full <- new.magpie(getCells(x), years, getNames(x))
  
  # convert to arrays to speed up performance
  full <- as.array(full)
  x    <- as.array(x)
  base <- as.array(base)
  
  # from start until ref_year, use the corresponding ref value
  full[,till_ref,]      <- base[,till_ref,]
  
  if(hard_cut){
    
    ###########################################
    ### Use GCM data after historical data  ###
    ### from reference year +1 on           ###
    ###########################################  
    
    full[,after_ref,] <- x[,after_ref,]
    
  } else if(limited==FALSE){
    
    full[,after_ref,]       <- base[,rep(ref_year,length(after_ref)),] * (x[,after_ref,] / x[,rep(ref_year,length(after_ref)),])
    
    full[,after_ref,][is.na(full[,after_ref,])] <- base[,rep(ref_year,length(after_ref)),][is.na(full[,after_ref,])]  # does this make sense?
    #full[is.infinite(full)]  <- toolFillYears(base[,ref_year,], after_ref) # does this make sense?
    
  } else {
    
    ###########################################
    ### Use DELTA-approach to put signal of ###
    ### GCM data on historical observation  ###
    ### data from reference year +1 on      ###
    ###########################################
    
    lambda <- sqrt(x[,ref_year,, drop=FALSE] / base[,ref_year,, drop=FALSE])
    lambda[base[, ref_year, ] <= x[, ref_year, ]] <- 1
    lambda[is.nan(lambda)] <- 1
    
    full[,after_ref,] <- base[,rep(ref_year,length(after_ref)),] * (1 + (x[,after_ref,] - x[,rep(ref_year,length(after_ref)),])/base[,rep(ref_year,length(after_ref)),] * 
                                           (base[,rep(ref_year,length(after_ref)),]/x[,rep(ref_year,length(after_ref)),]) ** lambda[,rep(ref_year,length(after_ref)),])
    
    full[,after_ref,][is.na(full[,after_ref,])]  <- 0
    
  }
  
  #check for nans and more
  if(any(is.infinite(full) | is.nan(full) | is.na(full))) stop("Data containing inconsistencies.")
  if(!negative) if(any(full<0)) stop("Data containing inconsistencies.")
  
  out <- as.magpie(full)
  
  return(out)
}
