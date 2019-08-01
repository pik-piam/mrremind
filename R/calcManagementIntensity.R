#' Calculate total tau
#' 
#' This function prepares total tau values for use. As the source data already
#' provides all required information this function purely removes unrequired
#' data and moves the xref values to the weighting object which is required for
#' aggregation.
#' 
#' 
#' @return Total tau data and corresonding weights as a list of two MAgPIE
#' objects
#' @author Benjamin Leon Bodirsky
#' @seealso \code{\link{calcTauTotal}}, \code{\link{readTau}},
#' \code{\link{convertTau}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ManagementIntensity")
#' 
#' }
#' 
calcManagementIntensity <- function() {
  
  past<-findset("past")
  tau<-collapseNames(calcOutput("LanduseIntensity",aggregate=F)[,past,"all"])
  snupe<-collapseNames(calcOutput("SNUpE",aggregate=F)[,past,"constant"])
  MI=setNames(1-snupe,"MI")
  TI=setNames(tau/MI,"TI")
    
  MITI <- mbind(MI,TI)
  
  #plotcountrymap(MITI[,"y2000","MI"]*10,catMethod=c(0:10))
  #plotcountrymap(MITI[,"y2000","TI"],catMethod=c(0:10))
  
  weight<-collapseNames(calcOutput("FAOLand",aggregate = FALSE)[,,"6620|Arable land and Permanent crops"])
  weight<-weight[,getYears(MITI),]
  
  warning("weighting by area or by production?")
  
  return(list(x=MITI,
              weight=weight,
              unit=".",
              description="Management Intensity and Technology Intensity as multipliers for a standardized LPJ yield",
              note=c("")))
}
