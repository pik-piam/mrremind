#' @title readResFor2ndBE
#' @description Read in old ReMIND use of residues for 2nd generation bioenergy and newly estimations
#'
#' @return List of magpie objects with results on old ReMIND regions level 
#' @param subtype oldReMIND, newAgriSupply
#' @author Kristine Karstens
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("ResFor2ndBE", subtype="oldReMIND")
#' }

readResFor2ndBE <- function(subtype){

  if(subtype == "oldReMIND"){
  
    residues_remind_EJ <- NULL
    load("residues_remind_EJ.Rdata")
    forest_share  <- 42.31301/100
    agri_share    <- 1-forest_share
    
    years_intersect <- intersect(findset("time"), getYears(residues_remind_EJ))
    
    ### unit conversion from EJ in PJ, use time subset
    residues_remind <- residues_remind_EJ[,years_intersect,] * 10^3
    
    ### divide into residues from agriculture and forestry
    out <- mbind(setNames(residues_remind * forest_share, "res_wood"),
                 setNames(residues_remind * agri_share,   "res_crop"))
    
    getSets(out) <- c("old_reg", "t", "residues")
  
  } else if(subtype == "newAgriSupply"){
    
    out <- read.magpie("ResAvailFor2ndBE.cs3")
    getSets(out) <- c("iso", "t", "kres","scenario")
  }
  
  return(out)
}
