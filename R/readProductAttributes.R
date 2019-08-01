#' Read product attributes
#' 
#' Read-in a file containing the attributes of MAgPIE products. Currently
#' Covers dry matter (DM), reactive nitrogen (Nr), Phosphorus (P),
#' Generalizable Energy (GE) and wet matter (WM). Values are assembled from
#' various literature sources, and the weighting and allocation is done in the
#' spreadsheet crop_specifications_06_2011.ods and
#' livestock_specifications_2012_06_14.ods in the svn folder /tools/Nutrients .
#' Values standardized on DM.
#' 
#' 
#' @param subtype Available subtypes: "Products", MAgPIE products "AgResidues"
#' Aboveground crop residues and "BgResidues" Belowground crop residues
#' @return magpie object with the dimension crops and attributes
#' @author Benjamin Leon Bodrisky
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("ProductAttributes")
#' }
#' @importFrom magclass read.magpie

readProductAttributes <- function(subtype="Products") {
  
  folder <- "Version_2017/"
  
  files <- c(Products        = "product_attributes.csv"       ,
             AgResidues      = "f_attributes_residue_ag.csv"  ,
             BgResidues      = "nr_residue_bg.csv"            ,
             LivingAnimals   = "attributes_living_animals.csv",
             SlaughterFactor = "SlaughterFactor.csv"          )
  
  
  file <- toolSubtypeSelect(subtype, files)
  
  if (subtype%in%c("SlaughterFactor","LivingAnimals")){
    vcat(3,"Living Animal attributes and slaughter factor not yet parametrized for fish.")
  }
  
  output          <- read.magpie(paste0(folder,file))
  getSets(output) <- c("region","year","attributes","products")
  
  if(subtype%in%c("Products","AgResidues","BgResidues","LivingAnimals")){
    output<-output/collapseNames(output[,,"dm"])
  }
  
  output<-collapseNames(output)
  
  return(output)
}

