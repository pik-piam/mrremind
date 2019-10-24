#' @title toolAWMSScenarioCreation
#' @description tool function to calculated the share of manure managed in different animal waste management systems in confinements.
#' @param name Name of the scenario
#' @param start_year Year were prediction starts
#' @param categories share of manure managed in different animal waste management systems
#' @param values target values
#' @param out contains historical data
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Edna J. Molina Bacca
#' @seealso
#' \code{\link{calcAWMSconfShr}}
#' 

toolAWMSScenarioCreation<-function(name,start_year,categories,values,out){
  
   lapply(lapply(values,sum), function(x) {if (x!=1) {stop("Categories do not sum one")}})
  
   years_list<-names(values)   
  
           for (i in seq_along(values)){
    
                          target_aim<-unlist(values[i],use.names = FALSE)
                          names(target_aim)<-categories
                          #These declarations account for the specific targets of each year of the scenario
                          year_target<-out[,start_year,"constant"]*target_aim["traditional"]
                          year_target[,,"digester"]<-year_target[,,"digester"]+target_aim["digester"]
                          year_target[,,"daily_spread"]<-year_target[,,"daily_spread"]+target_aim["daily_spread"]
                          
    
                                       if(i==1){
                                                 scenario_x<-convergence(origin=out[,,"constant"],aim=setYears(year_target,NULL),start_year = start_year,end_year = years_list[i],direction = NULL,type = "linear")
                                                 }else{
                                                 scenario_x<-convergence(origin=scenario_x,aim=setYears(year_target,NULL),start_year = years_list[i-1],end_year = years_list[i],direction = NULL,type = "linear")
                                                 }
                                         }
       getNames(scenario_x,dim=1)<-name
       return(mbind(out,scenario_x))
}