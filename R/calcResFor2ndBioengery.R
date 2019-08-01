#' @title calcResFor2ndBioengery
#' @description Calculates the supply potential of Crop Residues for 2nd generation bioenergy for future and different ssp scenarios
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @param products categorie (set) that should be reported, switch between "kres", "res_crop" (sum over all "kres"), "res_wood" and "all"
#' @param product_aggr boolean, if product set should be summed up 
#' @param add_off add a column with empty supply for no residues available for 2nd gen BE
#' @param extrapolation Extrapolate missing years (where values are zero)
#' @author Kristine Karstens
#' @seealso
#' \code{\link{calcResFor2ndBioengery}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ResFor2ndBioengery")
#' }

calcResFor2ndBioengery <- function(products="all", product_aggr=TRUE, add_off=FALSE, extrapolation=FALSE){
  
  if(!(products%in%c("kres","all","res_wood","res_crop"))) stop("This product type is not available.")
  mag_years  <- findset("time")
  past_years <- findset("past")
  
  oldReMIND     <- readSource("ResFor2ndBE", subtype="oldReMIND", convert=TRUE)
  missing_years <- setdiff(mag_years,getYears(oldReMIND))
  oldReMIND     <- time_interpolate(oldReMIND, missing_years, integrate_interpolated_years = TRUE, extrapolation_type = "constant")[,mag_years,]
  #set start years
  oldReMIND[,past_years,] <- 0
  
  newAgriSupply <- readSource("ResFor2ndBE", subtype="newAgriSupply", convert=TRUE)
  missing_years <- setdiff(mag_years,getYears(newAgriSupply))
  newAgriSupply <- time_interpolate(newAgriSupply, missing_years, integrate_interpolated_years = TRUE, extrapolation_type = "constant")[,mag_years,]
  #set start years
  newAgriSupply[,past_years,] <- 0
  
  
  if(products=="kres"){
    
    out <- newAgriSupply
    
  } else if(products=="res_crop"){
    
    out <- add_dimension(dimSums(newAgriSupply, dim=3.1), dim=3.1, nm="res_crop", add="residues")
    
  } else if(products=="res_wood"){
    
    out <- add_dimension(oldReMIND[,,"res_wood"], dim=3.2, nm=c("ssp1","ssp2","ssp3","ssp4","ssp5"), add="scenario")
      
  } else {
    
    out <- mbind(newAgriSupply, add_dimension(oldReMIND[,,"res_wood"], dim=3.2, nm=c("ssp1","ssp2","ssp3","ssp4","ssp5"), add="scenario"))
    
  }
  
  getSets(out) <- c("iso","t","residues","scenario")

  if(add_off){
    out          <- add_columns(out, addnm = c("off"), dim = 3.2)
    out[,,"off"] <- 0 
  }
  
  if(product_aggr) out <- dimSums(out, dim="residues")
  
  if (extrapolation) {
    #out <- time_interpolate()
  }
  
  return(list(x=out,
              weight=NULL,
              unit="except generalizable energy in PJ",
              description="Agricultural and forestry residues potentially available for 2nd generation bio energy")
  )
}
