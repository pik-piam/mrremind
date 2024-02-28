#' @title Prepare EDGETransport inputs
#'
#' @return magpie object of EDGEtransport iterative inputs
#' @author Alois Dirnaichner, Marianna Rottoli
#' @seealso \code{\link{readSource}}
#' @param subtype refer to the code for a list of allowed subtypes.
#'
#' @examples
#' \dontrun{ a <- calcOutput(type="EDGETransport", subtype="logit_exponent", aggregate=F)
#' }
#'
#'@importFrom data.table as.data.table
calcEDGETransport <- function(subtype) {

  switch(subtype,
         ".." = {
           weight = NULL
           unit = "[-]"
           description = ".."
         })

  return(list(x           = data,
              weight      = weight,
              unit        = unit,
              description = description))
}
