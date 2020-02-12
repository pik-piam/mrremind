#' convertCheung2018
#' 
#' @description Converts readCheung2018 output to complete MAgPIE object containing fishery data on aggregated FAO Major Fishing areas
#' @param subtype "General" data subtype. Areas in square km and Primary Production in mg C day^-1 for each Exclusive Economic Zone obtained from Seaaroundus.org
#' "ModelOutputDBEM" data subtype. DBEM Model output for RCP2.6;RCP8.5 obtained from Cheung et al 2018
#' "ModelOutputDynModel" data subtype. Dynamic Model output for RCP2.6;RCP8.5 obtained from Cheung et al 2018
#' @return Fishery data as complete MAgPIE object on country level
#' @author Benjamin Leon Bodirsky, Jasmin Wehner
#' @seealso \code{\link{readSource}}
#' @export
convertCheung2018 <- function(subtype){
   if (subtype == "General"){ # Reference Year (e.g. BAU, 2010)
     x_General <- readCheung2018(subtype = "General")
    return(x_General)
   }else if(subtype == "ModelOutputDBEM"){
     x_ModelOutputDBEM <- readCheung2018(subtype = "ModelOutputDBEM")
    return(x_ModelOutputDBEM)
  } else if(subtype == "ModelOutputDynModel"){
    x_ModelOutputDynModel <- readCheung2018(subtype = "ModelOutputDynModel")
   return(x_ModelOutputDynModel)
  }}
