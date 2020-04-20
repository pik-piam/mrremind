#' correctCheung2018
#' 
#' @description Converts readCheung2018 output to complete MAgPIE object containing fishery data on aggregated FAO Major Fishing areas
#' @param x unconverted magpie object from the read function.
#' @param subtype "General" data subtype. Areas in square km and Primary Production in mg C day^-1 or Mt yr^-1 for each Exclusive Economic Zone obtained from Seaaroundus.org
#' "models" data subtype: DBEM Model output for RCP2.6;RCP8.5 obtained from Cheung et al 2018
#' "ModelOutputDynModel" data subtype. Dynamic Model output for RCP2.6;RCP8.5 obtained from Cheung et al 2018
#' @return Fishery data as complete MAgPIE object on country level
#' @author Benjamin Leon Bodirsky, Jasmin Wehner
#' @seealso \code{\link{readSource}}
#' @export
correctCheung2018 <- function(x, subtype){
  
   if (subtype == "General"){ # Reference Year (e.g. BAU, 2010)
     #x <- readSource("Cheung2018",subtype = "General", convert=FALSE)
     x<-add_columns(x,addnm = "PrimProdMtYr", dim = 3.2)
     x[,,"PrimProdMtYr"]<-x[,,"PrimProdinmgCday"] * (10^-9) * 365 * 10^6
   } else if(subtype=="models") {
     #x <- readSource("Cheung2018",subtype = "models", convert=FALSE)
     x <- x *10^-2
     x[is.na(x)] <- 0
   } else {stop("unknown subtype")}

  return(x)
}
