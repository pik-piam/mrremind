#' @title calcBMIshr
#'
#' @description estimates average BMI of a BMI group for a population group
#'
#' @return List with a magpie object
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{readNCDrisc}},
#' \code{\link{calcIntake}},
#' \code{\link{readKuczmarski}}
#' 
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("BMI",aggregate=FALSE)
#' }
#' 
#' 

calcBMI <- function(){

  out<-readSource("WHObmi")
  
  return(list(x=out,
              weight=NULL,
              unit="kg/m^2",
              description="Body mass index of population belonging to a BMI group.",
              isocountries=FALSE))
}