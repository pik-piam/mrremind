#' @title calcAttributes
#' @description provides attributes of different products
#'
#' @param subtype subtype of readProductAttributes function.
#'
#' @return List of magpie objects with results on global level, empty weight, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{readProductAttributes}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("Attributes")
#' }
#' 



calcAttributes<-function(subtype="Products"){
  attributes<-readSource("ProductAttributes",subtype = subtype)
  return(list(x=attributes,weight=NULL,unit="t X per t dry matter (DM), except generalizable energy (ge), where it is PJ/Mt DM.",description="Values from literature survey. See SVN: tools/Nutrients/crop_specifications.ods or livestock_specifications.ods"))
}