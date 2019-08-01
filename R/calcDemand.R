
calcDemand<-function(){
  warning("This function is depreciated.")
  
  demand_input_country<-calcOutput("DemandCollectdata",aggregate = F)  
  koeppen<-readSource("Koeppen")
  combined<-NULL 
  combined_weight<-NULL
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP1",
    pop_scen="pop_SSP1",
    gdp_scen="gdp_SSP1", 
    urban_scen="urban_SSP1",
    dem_regr_type="log_log_decl_time",
    ls_regr_type="u_shape",  
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=3000, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2100",
    
    ls_aim=NULL, 
    ls_aim_type= "s",
    ls_aim_direction= "up",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP2",
    pop_scen="pop_SSP2",
    gdp_scen="gdp_SSP2", 
    urban_scen="urban_SSP2",
    dem_regr_type="log_log_decl_time",
    ls_regr_type="u_shape",
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=NULL, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2100",
    
    ls_aim=0.15, 
    ls_aim_type= "s",
    ls_aim_direction= "up",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP3",
    pop_scen="pop_SSP3",
    gdp_scen="gdp_SSP3", 
    urban_scen="urban_SSP3",
    dem_regr_type="log_log_lin_time",
    ls_regr_type="mult_lin_regr",
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=NULL, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2100",
    
    ls_aim=0.15, 
    ls_aim_type= "s",
    ls_aim_direction= "up",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP4",
    pop_scen="pop_SSP4",
    gdp_scen="gdp_SSP4", 
    urban_scen="urban_SSP4",
    dem_regr_type="log_log_decl_time",
    ls_regr_type="u_shape",  
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=NULL, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2100",
    
    ls_aim=0.15, 
    ls_aim_type= "s",
    ls_aim_direction= "up",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP5",
    pop_scen="pop_SSP5",
    gdp_scen="gdp_SSP5", 
    urban_scen="urban_SSP5",
    dem_regr_type="log_log_lin_time",
    ls_regr_type="mult_lin_regr",
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=NULL, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2100",
    
    ls_aim=0.15, 
    ls_aim_type= "s",
    ls_aim_direction= "up",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP1_boundary",
    pop_scen="pop_SSP1",
    gdp_scen="gdp_SSP1", 
    urban_scen="urban_SSP1",
    dem_regr_type="log_log_decl_time",
    ls_regr_type="u_shape",  
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=2588, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2050",
    
    ls_aim=0.1, 
    ls_aim_type= "s",
    ls_aim_direction= "down",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2050",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP2_boundary",
    pop_scen="pop_SSP2",
    gdp_scen="gdp_SSP2", 
    urban_scen="urban_SSP2",
    dem_regr_type="log_log_decl_time",
    ls_regr_type="u_shape",  
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=2750, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2050",
    
    ls_aim=0.15, 
    ls_aim_type= "s",
    ls_aim_direction= NULL,
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2050",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP3_boundary",
    pop_scen="pop_SSP3",
    gdp_scen="gdp_SSP3", 
    urban_scen="urban_SSP3",
    dem_regr_type="log_log_lin_time",
    ls_regr_type="mult_lin_regr",  
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=2933, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2050",
    
    ls_aim=0.2, 
    ls_aim_type= "s",
    ls_aim_direction= NULL,
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2050",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP4_boundary",
    pop_scen="pop_SSP4",
    gdp_scen="gdp_SSP4", 
    urban_scen="urban_SSP4",
    dem_regr_type="log_log_decl_time",
    ls_regr_type="u_shape",  
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=2588, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2050",
    
    ls_aim=0.1, 
    ls_aim_type= "s",
    ls_aim_direction= NULL,
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2050",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP5_boundary",
    pop_scen="pop_SSP5",
    gdp_scen="gdp_SSP5", 
    urban_scen="urban_SSP5",
    dem_regr_type="log_log_lin_time",
    ls_regr_type="mult_lin_regr",
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=2933, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2050",
    
    ls_aim=0.2, 
    ls_aim_type= "s",
    ls_aim_direction= NULL,
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2050",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  ### SRES scenarios
  
  scenarios<-toolDemandProjections(
    scenario_name = "a1",
    pop_scen="pop_a1",
    gdp_scen="gdp_a1", 
    urban_scen=NULL,
    dem_regr_type="log_log_lin_time",
    ls_regr_type="mult_lin_regr", 
    vfs_regr_type="koeppen",
    calib_year_start="y1990",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=NULL, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2100",
    
    ls_aim=NULL, 
    ls_aim_type= "s",
    ls_aim_direction= "up",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "a2",
    pop_scen="pop_a2",
    gdp_scen="gdp_a2", 
    urban_scen=NULL,
    dem_regr_type="log_log_lin_time",
    ls_regr_type="mult_lin_regr", 
    vfs_regr_type="koeppen",
    calib_year_start="y1990",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=NULL, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2100",
    
    ls_aim=NULL, 
    ls_aim_type= "s",
    ls_aim_direction= "up",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "b1",
    pop_scen="pop_b1",
    gdp_scen="gdp_b1", 
    urban_scen=NULL,
    dem_regr_type="log_log_decl_time",
    ls_regr_type="u_shape",  
    vfs_regr_type="koeppen",
    calib_year_start="y1990",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=NULL, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2100",
    
    ls_aim=NULL, 
    ls_aim_type= "s",
    ls_aim_direction= "up",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "b2",
    pop_scen="pop_b2",
    gdp_scen="gdp_b2", 
    urban_scen=NULL,
    dem_regr_type="log_log_decl_time",
    ls_regr_type="u_shape",  
    vfs_regr_type="koeppen",
    calib_year_start="y1990",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=NULL, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2100",
    
    ls_aim=NULL, 
    ls_aim_type= "s",
    ls_aim_direction= "up",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "history",
    pop_scen="pop_SSP2",
    gdp_scen="gdp_SSP2", 
    urban_scen="urban_SSP2",
    dem_regr_type="log_log_decl_time",
    ls_regr_type="u_shape",  
    vfs_regr_type="koeppen",
    calib_year_start="y2100",
    calib_year_end="y2100",
    calib_type="none",
    
    dem_aim=NULL, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2100",
    
    ls_aim=NULL, 
    ls_aim_type= "s",
    ls_aim_direction= "up",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP2_lowls",
    pop_scen="pop_SSP2",
    gdp_scen="gdp_SSP2", 
    urban_scen="urban_SSP2",
    dem_regr_type="log_log_decl_time",
    ls_regr_type="u_shape",  
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=NULL, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2100",
    
    ls_aim=0.15, 
    ls_aim_type= "s",
    ls_aim_direction= NULL,
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen  
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  
  
  scenarios<-toolDemandProjections(
    scenario_name = "SSP2_lowcal",
    pop_scen="pop_SSP2",
    gdp_scen="gdp_SSP2",  
    urban_scen="urban_SSP2",
    dem_regr_type="log_log_decl_time",
    ls_regr_type="u_shape", 
    vfs_regr_type="koeppen",
    calib_year_start="y2005",
    calib_year_end="y2100",
    calib_type="convergence",
    
    dem_aim=2750, 
    dem_aim_type= "s",
    dem_aim_direction= "down",
    dem_aim_startyear="y2010",
    dem_aim_endyear="y2050",
    
    ls_aim=0.15, 
    ls_aim_type= "s",
    ls_aim_direction= "up",
    ls_aim_startyear="y2010",
    ls_aim_endyear="y2100",
    demand_input_country=demand_input_country,
    koeppen=koeppen
  )  
  combined<-mbind(combined,scenarios[[1]])
  combined_weight<-mbind(combined_weight,scenarios[[2]])
  
  return(list(
    x=combined,
    weight=combined_weight,
    mixed_aggregation=TRUE,
    unit="Depends on indicator: Mio capita (hunger, pop), PJ (dem, l, mat, vegfruit), kcal_per_capita (kcal_pc), share (ls, hunger_shr, vegfruit_shr, urban_shr)",
    description="collection of food-demand related results of the food demand model. Will become obsolete with new mrfood library."))
}







