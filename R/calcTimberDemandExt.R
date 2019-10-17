#' @title calcTimberDemandExt
#' @description 
#' Calculates the demand of timber from FAO data (including intermediate products).
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{calcFAOmassbalance_pre}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("TimberDemandExt")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @export

calcTimberDemandExt <- function(){

x <- collapseNames(calcOutput("TimberDemand",aggregate = F)[,,"domestic_supply"][,,c("Industrial roundwood","Wood fuel")])
getNames(x) <- c("wood","woodfuel")
to_add <- c(paste0("y20",seq(20,95,5)),"y2100","y2105",paste0("y21",seq(10,50,5)))
ok_years <- intersect(getYears(x),findset("t_all"))
x <- x[,ok_years,]

count <- 0
for (i in to_add) {
  ## Make empty magpie object
  temp <- x[,"y1995",]
  getYears(temp) <- NULL
  temp <- setYears(x[,"y2015",],NULL) * 1.001^count
  getYears(temp) <- i
  count <- count+1
  x <- mbind(x,temp)
}

temp <- x
count <- 0
to_damp <- c(paste0("y20",seq(30,95,5)),"y2100","y2105",paste0("y21",seq(10,50,5)))
for (i in to_damp) {
  ## Make empty magpie object
  temp[,i,"woodfuel"] <- temp[,i,"woodfuel"] * 1/(1.05^count)
  count=count+1
}

final_demand <- round(temp,0)

return(list(x=final_demand,
            weight=NULL,
            min=0,
            unit="mio m3",
            description="Calculates the timber demand pattern based on historical FAO data"))

}
