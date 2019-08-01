#' @title calcGTAPTrade
#' @description calculate trade data from GTAP dataset
#' @param subtype   GTAP subtype
#' @param bilateral  whether bilateral trade data should be calculated
#' 
#' @return Trade related data as an MAgPIE object
#' @author Xiaoxi Wang
#' @examples
#'     \dontrun{
#'     x <- calcGTAP("GTAP7_VXMD")
#'     }

calcGTAPTrade <- function(subtype = NULL, bilateral = FALSE){
  out <- readSource(type ="GTAP",subtype = subtype)
  if(!bilateral & (length(fulldim(out)[[1]]) >3)){
     out <- dimSums(out,dim=3.1)
  }
  weight <- NULL
  unit <- "Mio.US$04"
  description <- subtype
  return(list(x=out,
              weight = weight,
              unit = unit,
              description = description))
}