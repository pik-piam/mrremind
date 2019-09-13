#' Calculate Fertilizer of N
#' 
#' Provides FertN data for N.No changes to the content have been done.
#' 
#' @param appliedto 'total' (default), 'crop' or 'past'
#' @param cellular cellular disaggreagation or national values
#' @param deposition for disaggregation will be passed on to calcNitrogenBudgetCropland
#' @param max_snupe for disaggregation will be passed on to calcNitrogenBudgetCropland
#' @return Fertilizer data for N and corresonding weights as a list of two
#' MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readIFA}},
#' \code{\link{convertIFA}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FertN")
#' 
#' }
#' @importFrom magpiesets findset
#' 
calcFertN <- function(appliedto="total",cellular=FALSE,deposition="CEDS",max_snupe=0.85) {
  fert <- readSource("IFA",subtype="consumption")[,,"Grand Total N", drop=TRUE]
  
  #split fertilizer into
  # application on cropland
  pastshare <- readSource("Lassaletta2014",subtype="fert_to_cropland")
  pastshare<-time_interpolate(pastshare,interpolated_year = setdiff(getYears(fert),getYears(pastshare)),integrate_interpolated_years = TRUE,extrapolation_type = "constant")
  
  # application on pasture
  # feed
  # use in other sectors
  
  if(appliedto=="crop"){
    fert <- fert*(1-pastshare)  
    
    if(cellular==TRUE){
      budget <- calcOutput("NitrogenBudgetCropland",include_fertilizer=FALSE,deposition=deposition,max_snupe=max_snupe,cellular=cellular,aggregate = FALSE)
      NUE <- calcOutput("SNUpE",max_snupe=max_snupe,aggregate = FALSE)[,findset("past"),"constant"]#, cellular=FALSE
      NUE <- NUE[getRegions(budget),,]
      NUE[NUE==0] <-  max_snupe   #default starting value
      
      withdrawals=dimSums(budget[,,c("harvest","ag","bg")],dim=3.1) - dimSums(budget[,,c("seed","fixation_crops")],dim=3.1)
      organicinputs=dimSums(budget[,,c("harvest","ag","bg","seed","fixation_crops","surplus","balanceflow"),invert=TRUE],dim=3.1)
    }
  } else if (appliedto=="past"){
    fert <- fert*pastshare
    if(cellular==TRUE){
      budget <- calcOutput("NitrogenBudgetPasture",include_fertilizer=FALSE,deposition=deposition,max_nue=max_snupe,cellular=cellular,aggregate = FALSE)
      NUE <- calcOutput("NuePasture",aggregate = FALSE)[,findset("past"),"constant"]#, cellular=FALSE
      NUE <- NUE[getRegions(budget),,]
      NUE[NUE==0] <-  max_snupe   #default starting value
      
      withdrawals=dimSums(budget[,,c("harvest")],dim=3.1)
      organicinputs=dimSums(budget[,,c("harvest","surplus","balanceflow"),invert=TRUE],dim=3.1)
    }
  } else if (appliedto=="total"){
    if(cellular==TRUE){stop("total not yet available on cellular level")}
    fert <- fert
  } else {stop("unknown appliedto")}
  
  if(cellular==TRUE){
    past<-findset("past")
    fert<-fert[,past,]
    
    mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)
    missing<-dimSums(fert,dim=1)-dimSums(fert[unique(mapping$iso),,],dim=1)
    vcat(verbosity = 2,paste0("Not all countries included in mapping. Fertilizer not accounted for in 2010 sums up to ",round(missing[,2010,],2)," Tg Nr"))
    
    fert = toolFertilizerDistribution(
      iteration_max=20, 
      max_snupe=0.85, 
      mapping=mapping, 
      from="celliso",
      to="iso", 
      fertilizer = fert, 
      SNUpE=NUE, 
      withdrawals=withdrawals, 
      organicinputs=organicinputs)
  }
  
  return(list(x=fert,
              weight=NULL,
              unit="Tg Nr",
              description="Fertilizer N (Grand total N) from IFA",
              isocountries=!cellular))
}
