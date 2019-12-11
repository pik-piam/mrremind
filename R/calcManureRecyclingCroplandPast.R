#' @title calcManureRecyclingCroplandPast
#' @description calculates manure recycling to cropland based on excretions, animal waste management types (and their shares per country) and emission factors for nitrogenous emissions in livestock confinements  
#'
#' @param cellular if TRUE value is calculate and returned (set aggregate to FALSE!) on cellular level
#' @param products "sum" (default) or "kli"
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @seealso
#' \code{\link{calcExcretion}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("calcManureRecyclingCroplandPast")
#' }
#' @importFrom magclass getNames<-


calcManureRecyclingCroplandPast <- function(products="sum", cellular = FALSE ){

  past               <- findset("past")
  Excretion          <- collapseNames(calcOutput("Excretion", cellular = cellular, aggregate = FALSE)[,past,"confinement"])
  EmissionFactors3   <- calcOutput("EF3confinement", selection="recycling", aggregate = FALSE)
  AnimalWasteMSShare <- collapseNames(calcOutput("AWMSconfShr", aggregate = FALSE)[,past,"constant"])
  
  if(cellular){
    
    EmissionFactors3   <- toolIso2CellCountries(EmissionFactors3)
    AnimalWasteMSShare <- toolIso2CellCountries(AnimalWasteMSShare)
  }
  
  if(products == "sum"){
    
    ManureNitrogen       <- dimSums(Excretion[,,"nr"] * AnimalWasteMSShare * EmissionFactors3, dim = c(3.1, 3.3))
    vcat(verbosity = 2,"no P and K losses in manure management assumed")
    ManurePhosphorKalium <- dimSums(Excretion[,,c("p","k")], dim = 3.1)
    
  } else if (products == "kli"){
    
    ManureNitrogen       <- dimSums(Excretion[,,"nr"] * AnimalWasteMSShare * EmissionFactors3, dim = c(3.3))
    vcat(verbosity = 2,"no P and K losses in manure management assumed")
    ManurePhosphorKalium <- Excretion[,,c("p","k")]
    
  } else stop(paste("Type", products ,"is not a valid for parameter 'products'."))
              
  out       <- mbind(ManureNitrogen, ManurePhosphorKalium)
  
  return(list(x            = out,
              weight       = NULL,
              unit         = "Mt Nr, P, K",
              description  = "Manure from confinements recycled to croplands",
              isocountries = !cellular)
  )    
}