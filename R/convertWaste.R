#' convertWaste
#' 
#' @description Converts readWaste output to complete MAgPIE object containing Waste data on country level (kg/cap)
#' @param subtype type of waste data, generation composition treatment or special
#' @return Waste data as complete MAgPIE object on country level
#' @author David Chen
#' @seealso \code{\link{readSource}}

convertWaste <- function(subtype){
  if (subtype=="Generation"){
  x<-readSource("Waste", subtype="Generation", convert=F)

  }
  else if (subtype=="Composition"){
  x<-readSource("Waste", subtype="Composition", convert=F)
    }
  else if (subtype=="Treatment"){
    x<-readSource("Waste", subtype="Treatment", convert=F)
    # tmp_regions <- where(dimSums(x, na.rm=T)==0)$false$regions #where data exists fill NA spots with 0, to say that these treatments are 0
    # x[c(tmp_regions),,][is.na(x[c(tmp_regions),,])] <- 0
    }
  else if (subtype=="Special"){
    x<-readSource("Waste", subtype="Special", convert=F)
  }
 x <- toolCountryFill(x, fill=NA)
 
 return(x)
}