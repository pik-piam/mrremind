#' @title calcResBiomass
#' @description Provides MAgPIE-FEED data for aboveground and belowground residues biomass
#' 
#' @param cellular If TRUE calculation and output on cellular level
#' @param plantparts both, ag (aboveground) or belowground (bg). Both can have memory problems for cellular outputs
#' @param irrigation if TRUE, distinguishes irrigated and non-irrigated crops
#' @param attributes in dm, wm, ge, nr, p, k
#' @return MAgPIE-FEED data for ProdResAg and corresonding weights as a list of
#' two MAgPIE objects
#' @author Lavinia Baumstark, Isabelle Weindl, Benjamin Bodirsky
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ResBiomass")
#' 
#' }
#' 
calcResBiomass <- function(cellular=FALSE, plantparts="both",irrigation=FALSE,attributes="all") {
  

  MAGcroptypes   <- findset("kcr")
  
  # memory problems for cellular data
  if(plantparts=="both"){
    AboveGroundResidues   <- calcOutput("ResBiomass",cellular=cellular,aggregate = FALSE,plantparts="ag", irrigation=irrigation,attributes=attributes)
    BelowGroundResidues   <- calcOutput("ResBiomass",cellular=cellular,aggregate = FALSE,plantparts="bg", irrigation=irrigation,attributes=attributes)
    ResidueProduction     <- mbind(AboveGroundResidues, BelowGroundResidues)
  } else if (plantparts=="ag") {
    # read in area harvested
    HarvestedArea  <- calcOutput("Croparea", sectoral="kcr", physical=FALSE, cellular=cellular, irrigation=irrigation, aggregate=FALSE)
    
    # read in production
    CropProduction <- collapseNames(calcOutput("Production", products="kcr", cellular=cellular, attributes="dm", irrigation=irrigation,aggregate = FALSE))
    
    # read harvest index 
    HarvestIndex   <- setYears(readSource("HI"), NULL)[,,MAGcroptypes] 
    
    # calculate residue production 
    ResWithProduction     <- CropProduction * collapseNames(HarvestIndex[,,"slope"])                         
    ResWithHarvestedArea  <- HarvestedArea  * collapseNames(HarvestIndex[,,"intercept"])                     
    
    AboveGroundResidues   <- (ResWithProduction   + ResWithHarvestedArea) 
    # read in residues attributes
    AttributesAboveGround <- readSource("ProductAttributes", subtype = "AgResidues")[,,MAGcroptypes]
    if(!all(attributes%in%"all")){# for problems with memory size
      AttributesAboveGround=AttributesAboveGround[,,attributes]
    }
    AboveGroundResidues   <- AboveGroundResidues * AttributesAboveGround
    ResidueProduction   <- add_dimension(AboveGroundResidues, dim = 3.1, add = "residues", nm = "ag") 
    
  } else if (plantparts=="bg") {
    # read harvest index 
    CropProduction <- collapseNames(calcOutput("Production", products="kcr", cellular=cellular,attributes="dm", irrigation=irrigation, aggregate = FALSE))
    AboveGroundResidues <- collapseNames(calcOutput("ResBiomass",cellular=cellular,plantparts="ag",attributes="dm", irrigation=irrigation,aggregate = FALSE))
    HarvestIndex   <- setYears(readSource("HI"), NULL)[,,MAGcroptypes] 
    BelowGroundResidues   <- (AboveGroundResidues + CropProduction) * collapseNames(HarvestIndex[,,"bg_to_ag"])
    # read in residues attributes
    AttributesBelowGround <- readSource("ProductAttributes", subtype = "BgResidues")[,,MAGcroptypes]
    
    if(!all(attributes%in%getNames(AttributesBelowGround,dim=1))){ # for problems with memory size
      ResidueProduction   <- BelowGroundResidues *  AttributesBelowGround
      # add all product attribute to below ground residues (p, k, ge, wm with 0) 
      AttributesBGResNames  <- getNames(AttributesBelowGround, dim=1)
      AttributesNames       <- findset("attributes")
      AttributesMissNames   <- setdiff(AttributesNames, AttributesBGResNames)  
      AttributesBelowGround   <- add_columns(AttributesBelowGround, addnm = AttributesMissNames, dim = 3.1)
      AttributesBelowGround[,,AttributesMissNames] <- 0
    } 
    if(!all(attributes%in%"all")) {
      AttributesBelowGround<-AttributesBelowGround[,,attributes]
    }
    
    ResidueProduction   <- BelowGroundResidues *  AttributesBelowGround
    ResidueProduction   <- add_dimension(ResidueProduction, dim = 3.1, add = "residues", nm = "bg") 

  } else {stop("unkown plantpart")}
  
  if(!all(attributes%in%"all")){# for problems with memory size
    ResidueProduction=ResidueProduction[,,attributes]
  }
  
  return(list(x            = ResidueProduction,
              weight       = NULL,
              unit         = "mio ton DM,Nr,P,K,WM",
              description  = "aboveground and belowground residues production",
              isocountries =!cellular)
        )
}
