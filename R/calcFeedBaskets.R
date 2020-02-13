#' @title calcFeedBaskets
#' @description Combines feed baskets of the past with scenario-dependent future feed baskets.
#'
#' @param non_eaten_food if TRUE, non-eaten food is included in feed baskets, if not it is excluded.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Isabelle Weindl, Benjamin Leon Bodirsky, Stephen Wirth, Jan Philipp Dietrich
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FeedBaskets")
#' }
#' @importFrom magclass getNames getYears add_columns add_dimension magpie_expand
#' @importFrom luscale rename_dimnames
#' @importFrom utils tail


calcFeedBaskets <- function(non_eaten_food=FALSE) {
  
  fbask_sys <- calcOutput("FeedBasketsSysPast", aggregate = FALSE)
  
  # create MAgPIE objects which contain 1 for entries belonging to the main share and 0 for entries belonging to the anti share
  belong2type <- function(commodities, elems) {
    mainshr_rum    <- c("res_cereals","res_fibrous","res_nonfibrous","pasture")
    antishr_pig    <- c("tece","trce","maiz","rice_pro",
                        "others","potato","cassav_sp","puls_pro",         
                        "soybean","rapeseed","groundnut","sunflower","oilpalm","cottn_pro",    
                        "sugr_beet","sugr_cane",
                        "livst_rum","livst_pig","livst_chick","livst_egg","livst_milk", "fish")
    mainshr_poultry <- c("distillers_grain", "molasses", "oilcakes", "brans")
    
    mainshr_pig <- commodities[-match(antishr_pig, commodities, nomatch = FALSE)]
    
    main <- new.magpie(cells_and_regions = "GLO", years = NULL, names = elems, fill = 0)
    
    const <- add_dimension(main, dim=3.3, add="type",nm="const")
    
    main[,,mainshr_rum][,,c("sys_dairy","sys_beef")] <- 1
    main[,,mainshr_pig][,,"sys_pig"] <- 1
    main[,,mainshr_poultry][,,c("sys_chicken","sys_hen")] <- 1
    
    anti <- add_dimension(1-main, dim= 3.3, add="type",nm="anti")
    main <- add_dimension(main, dim = 3.3, add="type", nm="main")
    
    #define feed commodities that do not change over time; they should not belong to the feed commodities
    #that define the main feed share used for the regression analysis
    constshr <- c(findset("kap"),"potato","puls_pro","sugr_beet","sugr_cane","groundnut")
    mselect(const,data1=constshr) <- 1
    mselect(anti,data1=constshr)  <- 0
    mselect(main,data1=constshr)  <- 0
    #additionally hold "brans" constant in ruminant feed baskets 
    #(not possible for pig system where brans are part of the main share):
    const[,,c("sys_beef.brans","sys_dairy.brans")] <-1
    anti[,,c("sys_beef.brans","sys_dairy.brans")] <-0
    out <- mbind(main,anti,const)
    if(!all(dimSums(out,dim=3.3)==1)) stop("Something went wrong assigning commodities to types. Each commodity needs to be assigned to exactly one type!")
    
    return(out)
  }
  
  # feed within the commodity group "const" is for all animal systems excluded from the transition process (from main_shr to antishr)
  ctype <- belong2type(commodities=getNames(fbask_sys, dim=2), elems=getNames(fbask_sys))
  
  past<-findset("past")
  year <- tail(past,1)
  
  #read in the ratio of livestock production allocated to the different systems 
  prod_sys_ratio <- calcOutput("ProdSysRatioPast", aggregate = FALSE)
  prod_sys_ratio <- toolHoldConstantBeyondEnd(prod_sys_ratio)
  
  #use livestock production as weight
  kli <- findset("kli")
  weight_kli <- collapseNames(calcOutput("FAOmassbalance_pre",aggregate = FALSE)[,,kli][,,"dm"][,,"production"])
  weight_sys <- dimSums(fbask_sys,dim=3.2)
  weight_sys[,,] <- 0
  for(t in past){
    weight_sys[,t,] <- dimSums(weight_kli[,t,]*prod_sys_ratio[,t,],dim=3.1)
  }
  weight_sys <- toolHoldConstantBeyondEnd(weight_sys)
  weight_kli <- setYears(weight_kli[,year,],NULL)
  
  
  
  calc_fbask_shr <- function(x, main, dim) {
    out <- dimSums(x*main,dim=dim)/dimSums(x,dim=dim)
    if(anyNA(out)) {
      replacement <- as.magpie(apply(out,3,mean, na.rm=TRUE))
      out <- toolNAreplace(out, replaceby=replacement)$x
    }
    return(out)
  }

  fbask_shr <- calc_fbask_shr(fbask_sys, ctype, dim=3.2)
    

  # Read in Central Feedshares and calibrate them to observed shares
  out_shr <- calcOutput("CentralFeedshares"   , aggregate=FALSE) 
  
  
  calib_shr <- function(fbask_shr, out_shr, start_year, end_year, type="linear") {
    fbask_shr <- toolHoldConstantBeyondEnd(fbask_shr)
    
    #calibration of main share:
    future <- getYears(fbask_shr)[-match(past, getYears(fbask_shr), nomatch = FALSE)]
    diffm <- magpie_expand(fbask_shr[,year,"main",drop=TRUE][,,getNames(out_shr, dim=1)],out_shr[,year,]) - out_shr[,year,]
    outm <- magpie_expand(fbask_shr[,,"main",drop=TRUE][,,getNames(out_shr, dim=1)],out_shr)
    outm[,future,] <- out_shr[,future,] + setYears(diffm,NULL)
    #(partial) convergence to regression values over a long time horizon:
    outm <- convergence(outm,out_shr, start_year=start_year, end_year=end_year, type=type)
    #set limit of 10% main share or the share in the last historical year in case it is lower than the limit
    outm[,future,] <- outm[,future,]*(outm[,future,]>0.1)+outm[,year,]*(outm[,future,]<0.1&outm[,year,]<0.1)+0.1*(outm[,future,]<0.1&outm[,year,]>0.1)

    # add missing systems
    missing <- setdiff(getNames(fbask_shr, dim=1),getNames(outm,dim=1))
    outm <- add_columns(outm,missing,dim=3.1)
    outm[,,missing] <- fbask_shr[,,"main",drop=TRUE][,,missing] 
    
    out <- add_dimension(fbask_shr, dim = 3.2, add = "scen", nm = getNames(outm, dim=2))
    out[,,"main"] <- outm
    out[,,"anti"] <- 1 - out[,,"main"] - out[,,"const"]
    #remove negative values:
    out[,,"anti"][which(out[,,"anti"]<0)] <- 0
    out[,,"const"] <- 1 - out[,,"main"] - out[,,"anti"]
    
    if(!all(round(dimSums(out,dim="type"),8)==1)) stop("Something went wrong calibrating the fbask shares!")  
    return(out)
  }
  cal_shr <- calib_shr(fbask_shr, out_shr, start_year=year, end_year=2050, type="linear")
  
  
  # Read in efficiencies and calibrate them
  out_eff <- calcOutput("FeedEfficiencyFuture", aggregate=FALSE)
  calc_multiplier <- function(x,year) {
    out <- x/setYears(x[,year,], NULL)
    out[is.nan(out) | is.infinite(out)] <- 1
    return(out)
  }
  mult_eff  <- calc_multiplier(out_eff, year)

  
  calc_fshare <- function(fbask_sys, ctype, cal_shr, weight_sys, year) {
    calc_converge_weight <- function(x, years=NULL, start=year, converge=TRUE) {
      calc_weight <- function(x, dim=3.2) {
        x <- x + dimSums(x,dim=1)/sum(x)*10^-10
        return(x/dimSums(x,dim=dim))
      }
      w <- calc_weight(x, 3.2)
      w <- setYears(w[,rep(1,length(years)),], years)
      if(converge) {
        # convergence to global mean weights (assuming that production systems will get more similar over time)
        wglo <- magpie_expand(calc_weight(dimSums(x*weight_sys[,getYears(x),],dim=1)/dimSums(weight_sys[,getYears(x),],dim=1), 3.2), w)
        
        w <- convergence(w, wglo, start_year=start, end_year=tail(years,1), type="linear")
      }
      if(!all(round(dimSums(w,dim=3.2),8)==1)) stop("Something went wrong in the weight calculation (sum!=1)!")
      return(w)
    }
    fbask_sys_ref <- setYears(fbask_sys[,year,],NULL)
    
    main_weight  <- calc_converge_weight(x=fbask_sys_ref*ctype[,,"main"], years=getYears(cal_shr), start=year, converge=FALSE)
    anti_weight  <- calc_converge_weight(x=fbask_sys_ref*ctype[,,"anti"],  years=getYears(cal_shr), start=year, converge=TRUE)
    #no convergence to global targets for systems where the main feed share stays constant over time: "sys_chicken","sys_hen"
    anti_weight[,,c("sys_chicken","sys_hen")]  <- calc_converge_weight(x=fbask_sys_ref[,,c("sys_chicken","sys_hen")]*ctype[,,c("sys_chicken","sys_hen")][,,"anti"],
                                                                       years=getYears(cal_shr), start=year, converge=FALSE)
    const_weight <- calc_converge_weight(x=fbask_sys_ref*ctype[,,"const"],  years=getYears(cal_shr), start=year, converge=FALSE)
    
    weight <- mbind(main_weight, anti_weight, const_weight)
    fshare <- dimSums(weight*cal_shr, dim=3.3) #problem with "type"
    fbask_sys_cal <- fshare*dimSums(fbask_sys_ref, dim=3.2)
    if(any(round(fbask_sys_cal[,year,] -fbask_sys[,year,],9)!=0)) stop("Something went wrong in the fbask calibration!")
    
    return(fbask_sys_cal)
  }
  fbask_sys_cal <- calc_fshare(fbask_sys, ctype, cal_shr, weight_sys, year)
  
  ###########application of total feed efficiency trends and main feed share trends
  data.sys <- fbask_sys_cal*mult_eff
  data.sys[,past,] <- fbask_sys
  
  # make sure that self-demand stays below a given threshold
  self_demand <- c("sys_pig.livst_pig","sys_beef.livst_rum", "sys_chicken.livst_chick", "sys_hen.livst_egg","sys_dairy.livst_milk")
  threshold <- 0.8
  if(any(data.sys[,,self_demand] >= threshold)) {
    stop("Some systems were demanding more than ", threshold*100, "% of itself as feed!")
  }
  
  
  #calculation of product-specific feed basket data (for livst_chick,livst_egg,livst_milk,livst_pig,livst_rum)
  data <-dimSums(prod_sys_ratio*data.sys,dim=3.1)
  
  
  #remove non_eaten_food if not established as product yet
  if(!non_eaten_food){
    data <- data[,,"non_eaten_food", invert=TRUE]
    weight_kli <- weight_kli[,,"non_eaten_food", invert=TRUE]
  }

  getSets(data, fulldim=FALSE)[3] <- "kli.k.scen"
  
  return(list(x=data,
              weight=weight_kli,
              unit="1",
              description="Detailed feed requirements in DM per DM products generated for 5 livestock commodities"))
}
