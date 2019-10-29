#' @title calcTimberHarvestCost
#' @description Calculates the cost for timber harvest
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("TimberHarvestCost")
#' }
#' @importFrom magclass setNames
calcTimberHarvestCost<-function(){
  
gtap <- readSource("GTAP","GTAP7_VFM",convert = F)
gtap_harv_forestry <- dimSums(collapseNames(gtap[,,"frs"][,,c("SkLab")]),dim=3) ##SkLab as proxy. Maybe not 100percent good proxy
map <-  toolGetMapping("GTAP7Mapping2016.csv", type = "regional", where="moinput")

out <- toolAggregate(gtap_harv_forestry, rel=map, from="Region.code", to="Country.code", 
                     dim=1, partrel = TRUE, verbosity=2)
out <- toolCountryFill(out,fill = 50)
out[out<50] <- 50

out <- setYears(out,NULL)

weight <- setYears(setNames(dimSums(calcOutput("LanduseInitialisation",land="old",aggregate = F)[,"y1995",c("forest","forestry")],dim=c(3)),NULL),NULL)

weight[weight<1] <- 1

return(list(x=out,
            weight=weight, 
            unit= "US$04/ha", 
            description="harvesting_costs_forestry"))
}