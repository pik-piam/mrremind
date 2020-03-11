#' @title calcClossConfinement
#' @description Carbon losses for livestock confinements
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ClossConfinement")
#' }
#' @importFrom madrat toolNAreplace


calcClossConfinement<-function(){
  
  EF3confinement <- calcOutput("EF3confinement", products="magpie", selection=NULL, aggregate = FALSE, supplementary = TRUE)
  nloss          <- 1-collapseNames(EF3confinement$x[,,"recycling"])
  excretion      <- collapseNames(EF3confinement$weight[,,"recycling"])
  closs          <- nloss
  closs[]        <- NA
  
  k_poultry      <- c("livst_egg", "livst_chick")
  k_mammals      <- c("livst_rum", "livst_milk", "livst_pig")
  k_solid        <- c("solid_storage", "drylot", "other") 
  
  closs[,,"lagoon"]       <- 0.80
  closs[,,"daily_spread"] <- 0.15
  closs[,,"pit_short"]    <- 0.30    
  closs[,,"pit_long"]     <- 0.45
  closs[,,"digester"]     <- 0.80
  closs[,,"liquid_slurry"]<- 0.20
  closs[,,k_poultry][,,k_solid] <- 0.27
  closs[,,k_mammals][,,k_solid] <- 0.335 + 0.43 * nloss[,,k_mammals][,,k_solid]

  out<-toolNAreplace(x = closs, weight=excretion, replaceby = 0)
  
  return(list(x=out$x,
              weight=out$weight,
              unit="share",
              description="share of carbon losses in confined animal waste management system")
  ) 
}
