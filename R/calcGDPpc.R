#' @title calcGDPpc
#' @description calculates GDP per capita
#'
#' @return list of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Abhijeet Mishra
#' @param gdp Can be switched between ppp and MER. Default is ppp.
#' @seealso
#' \code{\link{calcPopulation}},
#' \code{\link{calcGDPppp}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("GDPpc", gdp="ppp")
#' }
#' @importFrom magclass setNames

calcGDPpc<-function(gdp="PPP"){
  
  drivers<-calcOutput("CollectProjectionDrivers",aggregate = FALSE)
  pop=collapseNames(drivers[,,"pop"])

  if(gdp=="PPP"){
    gdp_pc=collapseNames(drivers[,,"gdp"]/pop)
    gdp_pc[is.nan(gdp_pc)]<-0
    unit="US Dollar 2005 equivalents in PPP per capita"
  } else  if(gdp=="MER"){
    gdp_pc=collapseNames(drivers[,,"gdpmer"]/pop)
    gdp_pc[is.nan(gdp_pc)]<-0
    unit="US Dollar 2005 equivalents in MER per capita"
  }

  return(list(x=gdp_pc,
              weight=pop,
              unit=unit,
              description="per capita income"
              )
           )
    

}