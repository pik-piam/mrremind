#' Calculate FEEDRequirement, NutrientDensity and YieldsLive based on
#' regression model
#' 
#' This intermediate-function prepares FEEDRequirement, NutrientDensity and
#' YieldsLive for calc*-functions. As the source data already provides all
#' required information this function purely selects the needed data.
#' 
#' 
#' @return Productivity, feed requirements and nutrient requirements as well as corresonding weights as a list of two MAgPIE
#' objects
#' @author Lavinia Baumstark, Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{readWirsenius_FEED}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("RegressionFEED")
#' 
#' }
#' @importFrom magclass getNames setNames
calcRegressionFEED <- function() {

  past<-findset("past")
  
  en    <- readSource("Wirsenius_FEED")
  yield <- calcOutput("LivestockProductivity", aggregate = F, future=F)[,past,]
  
  # internal function for regression 
  .energy_reg<-function(data,reg_type,reg_animal,scale=1) {
    #get data from yield for regressed with (stock or prod yiels) for reg_animal multiplied with scale
    x    <- setNames(yield[,,reg_type],NULL) * scale
    # get data from en for data (feed or nutrient) for reg_animal for commodities that end A times x to the power 
    # data from en for reg_animal for commodities that end B
    y    <- en[,,data,pmatch=TRUE][,,reg_animal,pmatch=TRUE][,,"A"]*x^en[,,data,pmatch=TRUE][,,reg_animal,pmatch=TRUE][,,"B"]
    out[,,data,pmatch=TRUE][,,reg_animal,pmatch=TRUE] <- collapseNames(y)
  #  weights_out[,,data,pmatch=TRUE][,,reg_animal,pmatch=TRUE]<- x^en[,,data,pmatch=TRUE][,,reg_animal,pmatch=TRUE][,,"B"]
    return(out)
  }
  
  # create empty out magpie object
  out <-  new.magpie(getRegions(yield),getYears(yield),getNames(collapseNames(en[,,"A"])))
  # fill out with results from internal function .energy_reg
  out <- .energy_reg(data="feed",reg_type="sys_beef",  reg_animal="beef_gr", scale=1000)
  out <- .energy_reg(data="feed",reg_type="sys_beef",  reg_animal="beef_rep", scale=1000)
  out <- .energy_reg(data="feed",reg_type="sys_chicken",reg_animal="chicken", scale=1000)
  out <- .energy_reg(data="feed",reg_type="sys_pig",reg_animal="pig",scale=1000)
  out <- .energy_reg(data="feed",reg_type="sys_beef",  reg_animal="dairy_gr", scale=1000)
  out <- .energy_reg(data="feed",reg_type="sys_hen",reg_animal="hen", scale=1000)
  out <- .energy_reg(data="feed",reg_type="sys_dairy",reg_animal="dairy_rep", scale=1000)
  
  out <- .energy_reg(data="nutrient",reg_type="sys_beef",reg_animal="beef_gr", scale=1000)
  out <- .energy_reg(data="nutrient",reg_type="sys_beef",reg_animal="beef_rep", scale=1000)
  out <- .energy_reg(data="nutrient",reg_type="sys_beef",reg_animal="dairy_gr", scale=1000)
  out <- .energy_reg(data="nutrient",reg_type="sys_dairy",reg_animal="dairy_rep", scale=1000)
  
  return(list(x=out,
              weight=NULL,
              unit=c("GJ per ton FM (feed)","GJ per ton DM (nutrient)"),
              description="feed energy and nutrient density requirements related with livestock productivity (yield)"
  ))
}
