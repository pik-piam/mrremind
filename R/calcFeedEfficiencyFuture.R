#' calcFeedEfficiencyFuture
#' @description 
#' Calculates future central feed shares for all livestock categories 
#' based on the results of a non-linear regression between historical 
#' central feed shares and livestock productivity and using Koeppen-
#' Geiger climate zones
#'
#'
#' @return Central feed shares and weights as list of two MAgPIE-objects
#' @author Isabelle Weindl, Benjamin Bodirsky, Stephen Wirth
#'
#' @examples 
#' \dontrun{ 
#' calcOutput("FeedEfficiencyFuture")
#' 
#' }
#' @importFrom magclass magpie_expand 
calcFeedEfficiencyFuture <- function(){
  
  #1. feed efficiency regression coefficients a,b
  #2. livestock productivity x
  
  #calc livst productivity
  lvst_prod <- calcOutput("LivestockProductivity", aggregate = FALSE)
  
  #read regression coefficients for central feed shares
  feed_eff_regr <- readSource("FeedEfficiencyReg")
  
  
  func=function(x,a,b,z){
    # x : stock or producer yield
    # a: Feed efficiency regression coefficient
    # b: Feed efficiency regression coefficient
    out<- a*x^b
    return(out)
  }
  
  #calculate feed efficiencies for 5 livestock commodities
  out_eff <- new.magpie(cells_and_regions = getRegions(lvst_prod), years = getYears(lvst_prod), names = getNames(lvst_prod),fill = NA, sets = getSets(lvst_prod))
  
  out_eff[,,"sys_dairy"]<-func(lvst_prod[,,"sys_dairy"],a=feed_eff_regr[,,"sys_dairy.A"],b=feed_eff_regr[,,"sys_dairy.B"])
  out_eff[,,"sys_beef"]<-func(lvst_prod[,,"sys_beef"],a=feed_eff_regr[,,"sys_beef.A"],b=feed_eff_regr[,,"sys_beef.B"])
  out_eff[,,"sys_pig"]<-func(lvst_prod[,,"sys_pig"],a=feed_eff_regr[,,"sys_pig.A"],b=feed_eff_regr[,,"sys_pig.B"])
  out_eff[,,"sys_hen"]<-func(lvst_prod[,,"sys_hen"],a=feed_eff_regr[,,"sys_hen.A"],b=feed_eff_regr[,,"sys_hen.B"])
  out_eff[,,"sys_chicken"]<-func(lvst_prod[,,"sys_chicken"],a=feed_eff_regr[,,"sys_chicken.A"],b=feed_eff_regr[,,"sys_chicken.B"])
  
  
  #use livestock production as weight
  kli<-findset("kli")
  massbalance<-calcOutput("FAOmassbalance_pre",aggregate = F)
  weight <- collapseNames(massbalance[,,kli][,,"dm"][,,"production"])
  
  mapping<-data.frame(
    kli=c( "livst_pig","livst_rum","livst_chick","livst_egg","livst_milk"),
    sys=c("sys_pig","sys_beef","sys_chicken","sys_hen","sys_dairy" ),
    stringsAsFactors = FALSE)
  
  weight <- rename_dimnames(weight,dim = 3,query = mapping,from = "kli", to="sys")
  weight <- toolHoldConstantBeyondEnd(weight)
  
  out <- toolNAreplace(out_eff, weight, replaceby=0, val.rm = 0)
   
  return(list(x=out$x,
              weight=out$weight,
              unit="ton DM per ton DM",
              description="Feed efficiency for dairy cattle, beef cattle, pigs, hens and broilers"
  ))
  
}