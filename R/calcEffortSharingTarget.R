#' @title calc Effort Sharing Target
#' @description provides region specific Effort Sharing Emission target
#'
#' @return target data magpie object
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EffortSharingTarget")
#' }
#' 

calcEffortSharingTarget <- function(){
  
  t <- readSource("Eurostat_EffortSharing",subtype="target")
  e <- readSource("Eurostat_EffortSharing",subtype="emissions")
  
  target <- as.vector(e[,2005,]) * (1 + t) / 1000
  
  getNames(target) <- "co2"
    
  #Returning capacity values
  return(list(x=target, weight=NULL,
              unit="GtCo2", 
              description="Effort sharing emission target"             
  )) 
}

