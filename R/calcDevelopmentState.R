#' @title calcDevelopmentState
#' @description Provides development state of a country or region. We use worldbank definitions by default: above 12746 USD per capita, its a high-income country, below 1045 its a low-income country, in between its a medium-income country.
#' 
#' @param upper Change upper limit (default: 12746)
#' @param lower Change lower limit (default: 1045)
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @seealso
#' \code{\link{calcGDPpc}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("DevelopmentState")
#' }
#' 

calcDevelopmentState<-function(upper = 12746, lower = 1045){
  
  gdp_pc <- calcOutput("GDPpc",aggregate = FALSE, supplementary = TRUE)
  weight <- collapseNames(gdp_pc$weight)
  gdp_pc <- collapseNames(gdp_pc$x)
  
  developed <- (gdp_pc-lower) / (upper-lower)
  developed[developed<0] <- 0
  developed[developed>1] <- 1
  
  return(list(x=developed,
              weight=weight, 
              unit="share",
              description="Development state according to worldbank definitions: 0 is a low income country with less than 1000 USD per capita per year, 1 is a high-income country with more than 10000 USD per capita per year. Intermediate values are interpolated lineraly, and indicate medium income countries. Population weighted.",
              min=0,
              max=1)
  )
}

