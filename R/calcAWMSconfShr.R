#' @title calcAWMSconfShr
#' @description calculates the share of manure managed in different animal waste management systems in confinements. Starting with IPCC 2005 values, turning into scenarios for the future.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcAWMSconfShrPast}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("AWMSconfShr")
#' }
#' @importFrom magpiesets findset
#' @importFrom magclass collapseNames add_columns add_dimension getNames<- setYears



calcAWMSconfShr<-function(){
  past<-findset("past")
  out<-calcOutput("AWMSconfShrPast",aggregate = FALSE)
  weight2<-collapseNames(calcOutput("Excretion",aggregate = FALSE)[,past,"confinement"][,,"nr"])

  out<-add_dimension(toolHoldConstantBeyondEnd(out),dim = 3.1,nm = "constant")
  weight2<-toolHoldConstantBeyondEnd(weight2)
  
  #dairy_cows: less daily spread, more liquid, slurry and solid
  
  systems_smallscale=c("daily_spread","pit_short")
  systems_both=c("drylot","other")
  systems_industrial=c("lagoon","liquid_slurry","solid_storage","pit_long","digester")
  
  scenario_creation<-function(name,start_year,digester_2050,daily_spread_2050,digester_2100,daily_spread_2100){
    traditional_2050=1-digester_2050-daily_spread_2050
    traditional_2100=1-digester_2100+daily_spread_2100
    y2050<-out[,start_year,"constant"]*traditional_2050
    y2050[,,"digester"]<-y2050[,,"digester"]+digester_2050
    y2050[,,"daily_spread"]<-y2050[,,"daily_spread"]+daily_spread_2050
    y2100<-out[,start_year,"constant"]*traditional_2100
    y2100[,,"digester"]<-y2100[,,"digester"]+digester_2100
    y2100[,,"daily_spread"]<-y2100[,,"daily_spread"]+daily_spread_2100
    scenario_x<-convergence(origin=out[,,"constant"],aim=setYears(y2050,NULL),start_year = start_year,end_year = "y2050",direction = NULL,type = "linear")
    scenario_x<-convergence(origin=scenario_x,aim=setYears(y2100,NULL),start_year = "y2050",end_year = "y2100",direction = NULL,type = "linear")
    getNames(scenario_x,dim=1)<-name
    return(mbind(out,scenario_x))
  }
  
  out<-scenario_creation(
    name="ssp1",
    start_year="y2010",
    digester_2050=0.4,
    daily_spread_2050=0.2,
    digester_2100=0.7,
    daily_spread_2100=0.2
  )
  out<-scenario_creation(
    name="ssp2",
    start_year="y2010",
    digester_2050=0.3,
    daily_spread_2050=0,
    digester_2100=0.6,
    daily_spread_2100=0
  )
  out<-scenario_creation(
    name="ssp3",
    start_year="y2010",
    digester_2050=0.2,
    daily_spread_2050=0,
    digester_2100=0.4,
    daily_spread_2100=0
  )

  out<-scenario_creation(
    name="ssp4",
    start_year="y2010",
    digester_2050=0.4,
    daily_spread_2050=0,
    digester_2100=0.7,
    daily_spread_2100=0
  )
  
  out<-scenario_creation(
    name="ssp5",
    start_year="y2010",
    digester_2050=0.4,
    daily_spread_2050=0,
    digester_2100=0.7,
    daily_spread_2100=0
  )
  
  out<-scenario_creation(
    name="a1",
    start_year="y2010",
    digester_2050=0.7,
    daily_spread_2050=0,
    digester_2100=1,
    daily_spread_2100=0
  )
  
  out<-scenario_creation(
    name="a2",
    start_year="y2010",
    digester_2050=0.2,
    daily_spread_2050=0,
    digester_2100=0.5,
    daily_spread_2100=0
  )
  
  out<-scenario_creation(
    name="b1",
    start_year="y2010",
    digester_2050=0.3,
    daily_spread_2050=0,
    digester_2100=0.4,
    daily_spread_2100=0
  )
  
  out<-scenario_creation(
    name="b2",
    start_year="y2010",
    digester_2050=0.2,
    daily_spread_2050=0,
    digester_2100=0.5,
    daily_spread_2100=0
  )
  
  return(list(x=out,
              weight=weight2,
              unit="share",
              description="share of excreted nitrogen within stables excreted in which awms",
              min=0,
              max=1)
  ) 
}