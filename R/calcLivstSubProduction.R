#' Calculate  Production allocated to livestock production subsystems
#' 
#' Provides the FAO Livestock Production data differentiated into livestock subsystems
#' in million ton wet matter (based on subsystem allocation from Stefan Wirsenius).
#' 
#' 
#' @return Livestock Production (WM) and corresonding weights as a list of
#' two MAgPIE objects
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{readFAO}},
#' \code{\link{readWirseniusSubsystems}}, \code{\link{calcFAOmassbalance}},
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LivstSubProduction")
#' 
#' }
#' @importFrom magclass setNames
calcLivstSubProduction <- function() {
  
#read in data (on wet matter basis)
  livst_prod <- calcOutput("LivstProduction", aggregate=FALSE)
  meat2milk <- readSource("WirseniusSubsystems","cattlemeat2milk")
  meat2egg <- readSource("WirseniusSubsystems","meat2egg")
  beefsys <- readSource("WirseniusSubsystems","feed_beefsys")
  
  
  subsys <- c("dairy_reproducer","dairy_growing","beef_reproducer","beef_growing",
             "pig_all","hen","chicken")
  kli<-findset("kli")
  past <- findset("past")
  
#allocation of FAO livestock production to livestock production subsystems
  out <- new.magpie(getRegions(livst_prod),getYears(livst_prod),subsys)
  out <- add_dimension(out,dim=3.2,add="data2",nm=kli)
  
  ###cattle
  #primary 
    out[,,"dairy_reproducer.livst_milk"] <- livst_prod[,,"livst_milk"]
  #secondary
    for(t in past){
      out[,t,"dairy_reproducer.livst_rum"] <- setYears(meat2milk[,"y1995","dairy_reproducer"],t)*livst_prod[,t,"livst_milk"]
      out[,t,"dairy_growing.livst_rum"] <- setYears(meat2milk[,"y1995","dairy_growing"],t)*livst_prod[,t,"livst_milk"]
    }
    #ensure that dairy meat production does not exceed total ruminant meat production
    calib.factor.rum <- new.magpie(getRegions(livst_prod),getYears(livst_prod),"livst_rum")
    calib.factor.rum <- livst_prod[,,"livst_rum"]/(setNames(out[,,"dairy_reproducer.livst_rum"],"livst_rum") + setNames(out[,,"dairy_growing.livst_rum"],"livst_rum"))
    calib.factor.rum[calib.factor.rum > 1] <- 1
    calib.factor.rum[is.na(calib.factor.rum)] <- 1
    calib.factor.rum[is.nan(calib.factor.rum)] <- 1
    
    out[,,"dairy_reproducer.livst_rum"] <- setNames(calib.factor.rum,"dairy_reproducer.livst_rum")*out[,,"dairy_reproducer.livst_rum"]
    out[,,"dairy_growing.livst_rum"] <- setNames(calib.factor.rum,"dairy_growing.livst_rum")*out[,,"dairy_growing.livst_rum"]
      
    #remaining: meat subsystem
    beef_res_prod<- new.magpie(getRegions(livst_prod),getYears(livst_prod),"livst_rum")
    beef_res_prod[,,"livst_rum"] <- livst_prod[,,"livst_rum"] - setNames(out[,,"dairy_reproducer.livst_rum"],"livst_rum") - setNames(out[,,"dairy_growing.livst_rum"],"livst_rum")
    beef_res_prod[beef_res_prod < 0] <- 0
    
    for(t in past){
      out[,t,"beef_reproducer.livst_rum"] <- setYears(beefsys[,"y1995","beef_reproducer"],t)*beef_res_prod[,t,"livst_rum"]
      out[,t,"beef_growing.livst_rum"] <- setYears(beefsys[,"y1995","beef_growing"],t)*beef_res_prod[,t,"livst_rum"]
    }
  
  ###pigs
  #only primary
    out[,,"pig_all.livst_pig"] <- livst_prod[,,"livst_pig"]
  
  ###poultry
  #primary
    out[,,"hen.livst_egg"] <- livst_prod[,,"livst_egg"]
  #secondary
    for(t in past){
      out[,t,"hen.livst_chick"] <- setYears(meat2egg[,"y1995",],t)*livst_prod[,t,"livst_egg"]
    }
    #ensure that laying hen meat production does not exceed total ruminant meat production
    calib.factor.chick <- new.magpie(getRegions(livst_prod),getYears(livst_prod),"livst_chick")
    calib.factor.chick <- livst_prod[,,"livst_chick"]/setNames(out[,,"hen.livst_chick"],"livst_chick")
    calib.factor.chick[calib.factor.chick > 1] <- 1
    calib.factor.chick[is.nan(calib.factor.chick)] <- 1
    
    out[,,"hen.livst_chick"] <- setNames(calib.factor.chick,"hen.livst_chick")*out[,,"hen.livst_chick"]
      
  #remaining: meat subsystem
    out[,,"chicken.livst_chick"] <- setNames(livst_prod[,,"livst_chick"],"chicken.livst_chick") - setNames(out[,,"hen.livst_chick"],"chicken.livst_chick")
    
      
  out[is.nan(out)] <- 0
  out[is.na(out)] <- 0
  out[out < 0] <- 0
  
  return(list(x=out,
              weight=NULL,
              unit="mio ton WM",
              description="FAO livestock production aggregated to MAgPIE categories")
  )
}
