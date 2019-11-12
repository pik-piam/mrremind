#' Calculate food waste
#' 
#' Provides aggregated household food waste in the units Mt DM, GE, K,  NR, P and WM.
#' 
#' 
#' @return food waste and corresponding weights as a list of two MAgPIE
#' objects
#' @author Isabelle Weindl, Benjamin Leon Bodirsky
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFoodWasteRecycle}}
#' @examples
#' 
#' \dontrun{ 
#' a <- calcOutput("HHFoodWaste")
#' }
#' 
calcHHFoodWaste<-function(){
  past<-findset("past")
  
  mb<-calcOutput("FAOmassbalance",aggregate = FALSE)
  demand<-collapseNames(mb[,,"households"])
  demand2<-calcOutput("Demand",aggregate = FALSE)[,past,"history"]
  waste_shr<-collapseNames(demand2[,,"waste_shr"])
  
  out<-demand*waste_shr
  out<-dimSums(out,dim=3.1)
  
  return(list(x=out,
              weight=NULL,
              unit="Mt dm, ge, k,  nr, p, wm",
              description="Household food waste")
  )    
}