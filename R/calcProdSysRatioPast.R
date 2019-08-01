#' Calculate historical distribution of livestock production across different systems based on output of MAgPIE_FEED model
#' 
#' @return Historical distribution of livestock production across different systems and corresonding weights as a list of two MAgPIE
#' objects
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{readFeedModel}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ProdSysRatioPast")
#' 
#' }
#' @importFrom magclass getNames
#' @importFrom luscale rename_dimnames

calcProdSysRatioPast <- function() {
  mag_years_past <- findset("past")
  
  #read in data
  prodsysratio <-  readSource(type="FeedModel",subtype="ProdSysRatio")
  
  #use livestock production as weight
  kli<-findset("kli")
  massbalance<-calcOutput("FAOmassbalance_pre",aggregate = F)
  weight <- collapseNames(massbalance[,,kli][,,"dm"][,,"production"])
  
  mapping<-data.frame(
    kli=c( "livst_pig","livst_rum","livst_chick","livst_egg","livst_milk"),
    sys=c("sys_pig","sys_beef","sys_chicken","sys_hen","sys_dairy" ),
    stringsAsFactors = FALSE)
  
  weight<-rename_dimnames(weight,dim = 3,query = mapping,from = "kli", to="sys")
  
  # remove datasets with NAs in weight/data
  prodsysratio<-toolNAreplace(x=prodsysratio,weight=weight,replaceby=0)
  weight=prodsysratio$weight
  out=prodsysratio$x
  
  
  return(list(x=out,weight=weight,
              unit="-",
              description="Detailed historical system-specific feed requirements in DM per DM products generated for 5 livestock commodities."))
}