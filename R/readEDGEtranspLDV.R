#' Read EDGE Transport LDV shares
#' 
#' Read-in EDGE Transport LDV shares csv file as magclass object
#' 
#' 
#' @return magpie object of EDGE_TranspLDV
#' @author Renato Rodrigues
#' @seealso \code{\link{readSource}}
#'
#' @examples
#' \dontrun{ a <- readSource(type="EDGEtranspLDV")
#' }
#' 
#' @importFrom magclass as.magpie
#' @importFrom reshape2 melt
#' 

readEDGEtranspLDV <- function() {

  df <- readRDS("shares_LDV_totliq_roadliq.RDS")
  df <- as.data.frame(melt(df, id.vars=c("iso","year")))
  colnames(df) <- c("region","period","variable","value")
  x <- as.magpie(df,spatial=1,datacol=4)
  
  return(x)
}  
