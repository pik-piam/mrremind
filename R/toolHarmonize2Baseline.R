#' toolHarmonize2Baseline
#'
#' @param x magclass object that should be set on baseline
#' @param base nagclass object for baseline
#' @param ref_year Reference year
#' @param limited if TRUE, for an underestimated baseline the signal is limited to he additive term rather than the multiplicative factor
#'                if FALSE, x will be put on the baseline as a pure multiplicative factor
#' @param hard_cut Switch to TRUE for data that can not be harmonized, but have to be glued together
#'
#' @return the averaged data in magclass format
#' @author Kristine Karstens
#'
#' @export

toolHarmonize2Baseline <- function(x, base, ref_year, limited=TRUE, hard_cut=FALSE){
  
  if(!is.magpie(x) | !is.magpie(base)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  
  # check if years are overlapping and refs is part of both time horizons
  
  
  # set years
  years       <- union(getYears(x), getYears(base))
  till_ref    <- getYears(base, as.integer=TRUE)
  till_ref    <- till_ref[till_ref <= ref_year]
  after_ref   <- getYears(x, as.integer=TRUE)
  after_ref   <- after_ref[after_ref > ref_year]
  
  
  #check if x and base are identical in dimension except time
  
  
  # create new magpie object with full time horizon
  all         <- new.magpie(getCells(x), years, getNames(x))
  
  # convert to arrays to speed up performance
  all  <- as.array(all)
  x    <- as.array(x)
  base <- as.array(base)
  
  # from start until ref_year, use the corresponding ref value
  all[,till_ref,]      <- base[,till_ref,]
  
  if(hard_cut){
    
    ###########################################
    ### Use GCM data after historical data  ###
    ### from reference year +1 on           ###
    ###########################################  
    
    all[,after_ref,] <- x[,after_ref,]
    
  } else {
    
    ###########################################
    ### Use DELTA-approach to put signal of ###
    ### GCM data on historical observation  ###
    ### data from reference year +1 on      ###
    ###########################################
    
    for (d1 in 1:dim(all)[1]) {
      for (d3 in 1:dim(all)[3]) {
        
        if((base[d1, ref_year, d3] <= x[d1, ref_year, d3]) | limited==FALSE){ 
          ### LAMBDA EQUAL 1
          # unlimited, relative case for non underestimatet baseline in reference year
          
          if(x[d1, ref_year, d3] == 0){ 
            all[d1, after_ref, d3] <- 0                                                                     # for zero baselines
          } else {
            all[d1, after_ref, d3] <- base[d1, ref_year, d3] * (x[d1, after_ref, d3] / x[d1, ref_year, d3]) # for non zero baselines
          } 
          
        } else {
          ### LAMBDA UNEQUAL 1
          # limitedcase for underestimatet baseline in reference year
          
          if(x[d1, ref_year, d3] == 0){
            all[d1, after_ref, d3] <- base[d1, ref_year, d3] + x[d1, after_ref, d3]                         # for zero baselines
          } else {
            lambda <- sqrt( x[d1, ref_year, d3] / base[d1, ref_year, d3] )                                  # for non zero baselines
            
            all[d1, after_ref, d3] <- base[d1, ref_year, d3] * 
              (1 + (x[d1, after_ref, d3] - x[d1, ref_year, d3])/base[d1, ref_year, d3] * 
                 (base[d1, ref_year, d3]/x[d1, ref_year, d3]) ** lambda )
          } 
        }
      }
    }
  }

  #check for nans
  if(any(all[is.infinite(all) | is.nan(all) | is.na(all)])) stop("Data containing inconsistencies.")
  
  out <- as.magpie(all)
  
  return(out)
}