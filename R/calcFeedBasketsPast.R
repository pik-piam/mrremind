#' Calculate historical feed baskets based on output of MAgPIE_FEED model
#' as DM feed biomass (different types of feed) needed per DM livestock products
#' 
#' 
#' @return Historical feed baskets and corresonding weights as a list of two MAgPIE
#' objects
#' @param non_eaten_food if TRUE, non-eaten food is included in feed baskets, if not it is excluded.
#' @author Isabelle Weindl, Benjamin Bodirsky
#' @seealso \code{\link{calcOutput}}, \code{\link{readFeedModel}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FeedBasketsPast")
#' 
#' }
#' @importFrom magclass getNames
calcFeedBasketsPast <- function(non_eaten_food=TRUE) {
  mag_years_past <- findset("past")
  kli<-findset("kli")
  kap<-findset("kap")
  Xkap<-findset("kap",alias=TRUE)
  massbalance<-calcOutput("FAOmassbalance_pre",aggregate = F)
  
  weight <- collapseNames(massbalance[,,kap][,,"dm"][,,"production"])
  getNames(weight)<-paste0("alias_",getNames(weight))

  #read in system-specific feed basket data (for sys_dairy,sys_beef,sys_pig,sys_hen,sys_chicken)
  fbask_sys <-  calcOutput("FeedBasketsSysPast", aggregate = FALSE)
  
  
  #read in the ratio of livestock production allocated to the different systems 
  prod_sys_ratio <- calcOutput("ProdSysRatioPast", aggregate = FALSE)
  
  #calculation of product-specific feed basket data (for livst_chick,livst_egg,livst_milk,livst_pig,livst_rum)
  fbask <-dimSums(prod_sys_ratio*fbask_sys,dim=3.1)
  
  #expand kli to kap (i.e. adding animal product "fish")
  missingproducts<-setdiff(kap,kli)
  getNames(fbask)<-paste0("alias_",getNames(fbask))
  fbask <- add_columns(fbask,addnm = paste0("alias_",missingproducts),dim=3.1)
  fbask[,,paste0("alias_",missingproducts)] <- 0
  data <-fbask
  
  #expand dim=3.2 to kall (add products like wood and woodfuel)
  kdiff         <- setdiff(findset("kall"),getNames(data,dim=2))
  if(length(kdiff)>0){
    data          <- add_columns(data,addnm = kdiff,dim=3.2)
    data[,,kdiff] <- 0
  }
  
  #remove non_eaten_food if not established as product yet
  if(non_eaten_food==FALSE){
    kall<-findset("kall")
    data<-data[,,kall]
  }
  

  # remove datasets with NAs in weight/data
  data<-toolNAreplace(x=data,weight=weight,replaceby=0)
  weight <- data$weight
  out <- data$x

  
  return(list(x=out,weight=weight,
          unit="1",
          description="Detailed historical feed requirements in DM per DM products generated for 5 livestock commodities."))
}