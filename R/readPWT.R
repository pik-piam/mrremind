#' Read PWT
#' 
#' Read-in an PWT data .xlsx file as magclass object
#' 
#' 
#' @return magpie object of the PWT data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="PWT")
#' }
#' 
readPWT<- function() {
  pwt <- as.data.frame(read_excel("pwt81.xlsx", sheet="Data"))
    
  pwt <- pwt[,c(-2,-3)] # remove country and currency_unit
                        # columns. should be done better?
  
  pwt <- pwt[,!grepl("i_", names(pwt))] # remove indicator columns, as
                                        # they are text based and
                                        # don't mix well with floating
                                        # values

  pwt <- as.magpie(pwt,datacol=3)
                   
  return(pwt)
}
