#' Calculate Pasture Yields
#' 
#' Provides Pasture yields defined as ratio of grazed biomass to grazed area
#' 
#' 
#' @return Pasture yields and corresonding weights as a list of
#' two MAgPIE objects
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOmassbalance}},
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("PastureYield")
#' 
#' }
#' 
calcPastureYield <- function() {
  mag_years_past <- findset("past")
  
  biomass <- calcOutput("FAOmassbalance", aggregate=FALSE)[,,"production.dm"][,mag_years_past,"pasture"]
  biomass <- collapseNames(biomass)  
  
  past.land<-calcOutput("LanduseInitialisation", aggregate=FALSE)[,mag_years_past,"past"]
  
  p.yield<-biomass/past.land
  p.yield[is.nan(p.yield)]<-1
  p.yield[p.yield>100]<-100
  
  getNames(p.yield)<-NULL
  
  return(list(x=p.yield,
              weight=past.land,
              unit="ton DM per ha",
              description="Pasture yields")
  )
}