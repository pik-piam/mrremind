#' @title calcMilk
#' @description test for different cell namings
#' @param isocells if TRUE iso cells (JPN.2346) instead of global cell (GLO.1) are used
#'
#' @return List of magpie object
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("calcDump")
#' }
#' @importFrom magclass getCells<- getCells

calcMilk <- function(isocells = FALSE){
  
  dump <- calcOutput("Production", products = "kli", cellular = TRUE, aggregate = FALSE)[,,"wm"][,,"livst_milk"]
  
  if(!isocells){getCells(dump) <- paste0("GLO", substring(getCells(dump),4))}
  
  return(list(x      = dump,
              weight = NULL,
              unit   = "Mt WM",
              description="Cellular milk production"
          )
  )
  
}

