#' @importFrom magclass mbind getYears convergence calibrate_it dimSums
#' @importFrom magclass unwrap new.magpie is.magpie
toolDemandProjections<-function(
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
  
  demand_input_country,
  koeppen
) 
{
  

  ### helpfunctions for total demand, livestock and vegetables
  
  demand_kcal_regression<-function(gdp_pc,type="") 
  {
    if (!is.magpie(gdp_pc)) 
    {
      stop("gdp_pc has to be a magpie object")
    }
    if (length(dim(unwrap(gdp_pc)))!=3) 
    {
      stop("gdp_pc has to be a 3d object")
    }
    if (type=="log_log_lin_time") 
    {
      regr_c<- 2.8251159        # numbers taken from nlsregression Simulations
      regr_d<- 2.1313756*10^(-3)
      regr_e<- 0.1622219
      regr_f<- -3.12439*10^(-05)
      years<-new.magpie(cells_and_regions="GLO", years=getYears(gdp_pc),names="NULL",fill=getYears(gdp_pc,as.integer=TRUE))
      regr_a  <- exp(regr_c + regr_d * years)
      regr_b  <- regr_e + regr_f * years
      dummy<-(gdp_pc*0+1)*regr_b
      out<-regr_a * gdp_pc ^ (regr_b)
      #  exp(2.8251159+2.1313756*10^(-3)*2100) * 60000 ^ (0.1622219 + 3.12439*10^(-05) * 2100)
    } 
    else if (type=="log_log_decl_time") 
    {
      regr_am1 <- 387.473708
      regr_am2 <- 9.774726
      regr_am3 <- 933.888522
      regr_bm1 <- 0.008445119
      regr_bm2 <- -0.755692561
      regr_bm3 <- 0.08940805
      
      years<-new.magpie(cells_and_regions="GLO", years=getYears(gdp_pc),names="NULL",fill=getYears(gdp_pc,as.integer=TRUE))-1960
      regr_a  <- regr_am3 + (regr_am1 * years)/(years+regr_am2)
      regr_b  <- (regr_bm3 + (regr_bm1 * years)/(years+regr_bm2))
      out <- regr_a * gdp_pc ^ regr_b
      #   exp(2.8251159+2.1313756*10^(-3)*2100) * 60000 ^ (0.1622219 + 3.12439*10^(-05) * 2100)
      
    } 
    else if (type=="log_log_no_time") 
    {
      
      regr_a<- exp(7.074079)
      regr_b<- 0.099321
      out <-  regr_a * gdp_pc ^ regr_b
    } 
    else 
    {
      stop("unknown regression type")
    }
    return(out)
  }
  
  demand_ls_regression<-function(gdp_pc,type="u_shape") 
  {
    length(dim(unwrap(gdp_pc)))
    if (type=="u_shape") 
    {
      regr_p1 <- 1.371507*10^(-2)
      regr_p2 <- -5.295249*10^(-6)
      regr_q1 <- -1.102410*10^(-4)
      regr_q2 <- 6.403996*10^(-8)
      years<-new.magpie(cells_and_regions="GLO", years=getYears(gdp_pc),names="NULL",fill=getYears(gdp_pc,as.integer=TRUE))
      out <- (regr_p1 + regr_p2 * years)*((gdp_pc)^(0.5))*exp(-(regr_q1 + regr_q2 * years) * gdp_pc)
      
    } 
    else if (type=="mult_lin_regr") 
    {
      regr_a <- -36.732779363
      regr_b <- 4.497483702
      regr_c <- 0.016039027
      regr_d <- -0.002077227
      years<-new.magpie(cells_and_regions="GLO", years=getYears(gdp_pc),names="NULL",fill=getYears(gdp_pc,as.integer=TRUE))
      out <-  exp(regr_a + regr_b * log(gdp_pc) + regr_c*years + regr_d * log(gdp_pc)*years)
    } 
    else 
    {
      stop("unknown regression type")
    }
    return(out)
  }  
  
  demand_vfs_regression<-function(gdp_pc,type="koeppen") 
  {
    length(dim(unwrap(gdp_pc)))
    if (type=="koeppen") 
    {
      S_a=koeppen
      S_a=dimSums(S_a[,,c("kg_p_af","kg_p_aw","kg_p_bs","kg_p_cf","kg_p_df","kg_p_e")],dim=3)
      #Tropical rainforest climate, Tropical savannah climate, Steppe climate, Mild humid climate with no dry season, Snowy-forest climate with a moist winter, Polar ice climate
      S_a[is.na(S_a)]<-0.5
      #assume 0.5 for all mini countries not in the list
      v1=0.0433
      v2=385.256
      v3=0.0771
      v4=438.293
      out<-S_a*(v1*gdp_pc)/(v2+gdp_pc)+(1-S_a)*(v3*gdp_pc)/(v4+gdp_pc)
    } 
    else 
    {
      stop("unknown regression type")
    }
    return(out)
  }  
  
  
  ### readin
  gdp<-setNames(demand_input_country[,,gdp_scen],NULL)
  pop<-setNames(demand_input_country[,,pop_scen],NULL)
  if (!is.null(urban_scen))
  {
    urban<-setNames(demand_input_country[,,urban_scen],NULL)
  } 
  else 
  {
    urban<-pop
    urban[,,]<-NA
  }
  gdp_pc <- gdp/pop # pro Kopf Einkommen
  urban_shr <- urban/pop # urban share
  kcal_pc_calib_to = setNames(demand_input_country[,,"kcal_fao"],NULL) / 
                     setNames(demand_input_country[,,"pop_hist"],NULL)
  ls_calib_to = setNames(demand_input_country[,,"livst_kcal_fao"],NULL) / 
                setNames(demand_input_country[,,"kcal_fao"],NULL)
  vfs_calib_to = setNames(demand_input_country[,,"vegfruit_kcal_fao"],NULL) / 
                 setNames(demand_input_country[,,"kcal_fao"],NULL)
  #fish_pc_calib_to = setNames(demand_input_country[,,"fish_kcal_fao"],NULL)  / setNames(demand_input_country[,,"pop_hist"],NULL)
  material_calib_to = setNames(demand_input_country[,,"material"],NULL)
  
  ###checks
  
  ### demand projections
  # Apply regression on country data <- function
  
  kcal_pc_regr <- demand_kcal_regression(gdp_pc,type=dem_regr_type)
  ls_regr   <- demand_ls_regression(gdp_pc,type=ls_regr_type)
  vfs_regr   <- demand_vfs_regression(gdp_pc,type=vfs_regr_type)
  
  # Calibrate country data
  
  
  kcal_pc_calib <- calibrate_it(
    origin=kcal_pc_regr, 
    cal_to=setNames(kcal_pc_calib_to,NULL), 
    cal_type=calib_type,
    cal_year=calib_year_start, 
    end_year=calib_year_end, 
    report_calibration_factors=FALSE)
  
  ls_calib <- calibrate_it( # livestock
    origin=ls_regr, 
    cal_to=setNames(ls_calib_to,NULL), 
    cal_type=calib_type,
    cal_year=calib_year_start, 
    end_year=calib_year_end, 
    report_calibration_factors=FALSE)
  
  vfs <- calibrate_it( # fruit vegetables
    origin=vfs_regr, 
    cal_to=setNames(vfs_calib_to,NULL), 
    cal_type=calib_type,
    cal_year=calib_year_start, 
    end_year=calib_year_end, 
    report_calibration_factors=FALSE)
  
  
  # dem_aim_convergence
  
  if(!is.null(dem_aim)) 
  {
    
    kcal_pc <- convergence(
      origin=      kcal_pc_calib,
      aim=         dem_aim,
      type=        dem_aim_type,
      start_year=  dem_aim_startyear,
      end_year=    dem_aim_endyear,   
      direction=   dem_aim_direction
    ) 
  } 
  else 
  {
    kcal_pc <- kcal_pc_calib
  } 
  
  if(!is.null(ls_aim)) 
  {
    ls <- convergence(
      origin=      ls_calib,
      aim=         ls_aim,
      type=        ls_aim_type,
      start_year=  ls_aim_startyear,
      end_year=    ls_aim_endyear,   
      direction=   ls_aim_direction
    ) 
  } 
  else 
  {
    ls <- ls_calib
  } 
  
  
  ### calculate total demand
  
  dem <- kcal_pc * pop        #total demand
  l   <- kcal_pc * pop * ls   #livestock share
  vf  <- kcal_pc * pop * vfs  #vegetable and fruit share
  
  # material demand projections
  # assumed to grow proportional to food demand
  
  mat <- calibrate_it(
    origin=dem, 
    cal_to=setNames(material_calib_to,NULL), 
    cal_type="growth_rate",
    cal_year=calib_year_start, 
    report_calibration_factors=FALSE)
  #add history
  history<-1:which(getYears(dem)==calib_year_start)
  mat[,history,]<-material_calib_to[,history,]
  
  #fish<-dem
  #fish[,,] <- NA
  #fish[,,] <- setYears((fish_calib_to * pop)[,calib_year_start,],NULL)
  
  ### Add History before calibration point 
  
  if (scenario_name=="history")
  {
    years<-1:length(getYears(demand_input_country))
  } 
  else 
  {
    years<-1:(which(getYears(demand_input_country)==calib_year_start)-1)
  }
  
  gdp[,years,]<-setNames(demand_input_country[,years,"gdp_hist"],NULL)
  pop[,years,]<-setNames(demand_input_country[,years,"pop_hist"],NULL)
  urban[,years,]<-setNames(demand_input_country[,years,"urban_hist"],NULL)
  kcal_pc[,years,]<-setNames(demand_input_country[,years,"kcal_fao"],NULL) / setNames(demand_input_country[,years,"pop_hist"],NULL) 
  #fish[,years,]<-(fish_calib_to * pop )[,years,]
  dem[,years,]<-kcal_pc[,years,]*pop[,years,]    # total demand
  l[,years,]<-setNames(demand_input_country[,years,"livst_kcal_fao"],NULL) # livestock
  ls[,years,]<-l[,years,]/dem[,years,] # livestock share
  vf[,years,]<-setNames(demand_input_country[,years,"vegfruit_kcal_fao"],NULL) # vegetable
  vfs[,years,]<-vf[,years,]/dem[,years,] # vegetable share
  
  gdp_pc[,years,]<-gdp[,years,]/pop[,years,]  
  urban[,years,]<-urban[,years,]/pop[,years,]  
  
  # waste shr: at least 15% and all above 2200 kcal
  
  waste_shr=((kcal_pc>(2200/0.85))*(kcal_pc-2200)+(kcal_pc<=(2200/0.85))*(kcal_pc*0.15))/kcal_pc
  waste=((kcal_pc>(2200/0.85))*(kcal_pc-2200)+(kcal_pc<=(2200/0.85))*(kcal_pc*0.15))*pop
  
  ### Undernourishment
  
  hunger_shr <- 2674.855 * 0.997916997^kcal_pc / 100 
  hunger_shr[hunger_shr>1]<-1
  hunger <- hunger_shr * pop
  
  ### Aggregate to region  
  
  ### change units
  # kcal per day --> PJ per year
  #      1       --> 4.184 * 365 / 1000000
  dem = dem * 4.184*365/1000000
  l   = l * 4.184*365/1000000
  vf   = vf * 4.184*365/1000000
  #fish = fish * 4.184*365/1000000
  waste = waste * 4.184*365/1000000  
  
  ### prepare output
  
  combine_it<-function(add_x,to_y,varname,scenname) 
  {
    dimnames(add_x)[[3]]<-paste(varname,scenname,sep=".")
    out<-mbind(add_x,to_y)
    return(out)
  }
  
  message(paste0("Beginn processing the scenario ",
                 scenario_name,"..."))
  
  country<-NULL
  
  country<-combine_it(dem,country,"dem",scenario_name) #total demand
  country<-combine_it(ls,country,"ls",scenario_name) #livestock share
  country<-combine_it(vfs,country,"vegfruit_share",scenario_name) #vegetable fruit share
  country<-combine_it(pop,country,"pop",scenario_name)
  country<-combine_it(urban,country,"urban",scenario_name)
  country<-combine_it(urban_shr,country,"urban_shr",scenario_name)
  country<-combine_it(gdp,country,"gdp",scenario_name)
  country<-combine_it(gdp_pc,country,"gdp_pc",scenario_name)
  country<-combine_it(kcal_pc,country,"kcal_pc",scenario_name)
  country<-combine_it(l,country,"l",scenario_name)
  country<-combine_it(vf,country,"vegfruit",scenario_name)
  country<-combine_it(mat,country,"mat",scenario_name)
  #  country<-combine_it(fish,country,"fish",scenario_name)  
  country<-combine_it(waste,country,"waste",scenario_name)    
  country<-combine_it(waste_shr,country,"waste_shr",scenario_name)  
  country<-combine_it(hunger,country,"hunger",scenario_name) 
  country<-combine_it(hunger_shr,country,"hunger_shr",scenario_name) 
  
  country_weight<-country
  country_weight[,,c("dem","pop","urban","gdp","l","vegfruit","mat","waste","hunger")] <- NA
  country_weight[,,c("gdp_pc","kcal_pc","hunger_shr","urban_shr")] <- setNames(country_weight[,,c("pop")],NULL)
  country_weight[,,c("ls","vegfruit_share","waste_shr")] <- setNames(country_weight[,,c("dem")],NULL)
  
  # turn NAs to 0
  country[is.na(country)] <- 0
  country_weight[is.na(country)] <- 0
  country[is.infinite(country)] <- 0
  country_weight[is.infinite(country_weight)] <- 0
  
  message(paste0("Finished processing the scenario ",
                 scenario_name,"."))
  
  return(list(x=country,weight=country_weight))
}
