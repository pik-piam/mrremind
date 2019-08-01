#' @title calcGTAP
#' @description calculate total value of trade margins from GTAP dataset
#' @param gtap_version type of GTAP data version
#' \itemize{
#' \item \code{GTAP7}
#' \item \code{GTAP8}
#' }
#' @param subtype   GTAP subtype
#' @param bilateral  whether bilateral trade margin should be calculated
#' 
#' @return Trade margins as an MAgPIE object
#' @author Xiaoxi Wang
#' @examples
#'     \dontrun{
#'     x <- calcGTAP("GTAP7","VXMD")
#'     }

calcGTAP <- function(gtap_version ="GTAP7",subtype = NULL, bilateral = FALSE){
  out <- readSource(type ="GTAP",subtype = paste(gtap_version,subtype,sep="_"))
  if(!bilateral & (length(fulldim(out)[[1]]) >3)){
     out <- dimSums(out,dim=3.1)
  }
  weight <- NULL
  unit <- "Mio.US$04"
  description <- paste(subtype,"obtained from", gtap_version, sep =" ")
  return(list(x=out,
              weight = weight,
              unit = unit,
              description = description))
}