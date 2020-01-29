#' @title calcRegressionParameters
#' @description Writes down the equation parameters from various data sources
#'
#' @param regression bmi_shr: Shares of BMI withina population. schofield or FAO_WHO_UNU1985: calculates intake based on anthropometrics
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("RegressionParameters")
#' }
#' 

calcRegressionParameters<-function(regression="bmi_shr")
{
  if(regression=="bmi_shr") {
    x <- readSource("Bodirsky2018",convert = FALSE,subtype="bmi_shr")
    description = "Equation parameters for BMI shares estimates using regression analysis. See Bodirsky et al 2019."
  } else if(regression=="demand_regression") {
    x <- readSource("Bodirsky2018",convert = FALSE,subtype="demand_regression")
    description = "Equation parameters for BMI shares estimates using regression analysis. See Bodirsky et al 2019."
  } else if(regression=="intake_regression") {
    x <- readSource("Bodirsky2018",convert = FALSE,subtype="intake_regression")
    description = "Equation parameters for intake using regression analysis."    
  } else if(regression=="bodyheight_regression") {
    x <- readSource("Bodirsky2018",convert = FALSE,subtype="bodyheight_regression")
    description = "Equations parameters for bodyheight regression. See Bodirsky et al 2019."    
  } else if (regression=="Schofield") {
    x <- readSource("Schofield",convert=FALSE)  
    description = "Equation parameters for intake estimates using regression analysis"
  } else if (regression=="FAO_WHO_UNU1985") {
    x <- readSource("FAO_WHO_UNU1985",convert = FALSE)
    description = "Equation parameters for intake estimates using regression analysis"
  } else if (regression=="Froehle") {
    x <- readSource("Froehle",convert = FALSE)
    description = "Equation parameters for intake estimates using regression analysis"    

  } else {stop("unknown type")}
  
  return(list(x = x,
              weight =  NULL,
              unit = "dimensionless",
              description = description,
              isocountries = FALSE)
  )
} 

