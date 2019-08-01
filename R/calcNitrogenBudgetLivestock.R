#' @title calcNitrogenBudgetLivestock
#' @description Calculates Nitrogen Budgets for the livestock sector on country levels.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NitrogenBudgetLivestock")
#' }
#' @importFrom magclass setNames



calcNitrogenBudgetLivestock<-function(){
  mb<-collapseNames(calcOutput("FAOmassbalance",aggregate = TRUE))
  kap<-findset("kap")
  feed<-mb[,,c("feed_fish","feed_livst_chick","feed_livst_egg","feed_livst_milk","feed_livst_pig","feed_livst_rum")]
  products<-mb[,,kap][,,"production"]
  ###stump
  stop("stump")
  out<-0
  #dimSums(outputs,dim=c(1,3))/dimSums(inputs,dim=c(1,3))
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Nr",
    description="Nitrogen budget on croplands for historical period"))
}

