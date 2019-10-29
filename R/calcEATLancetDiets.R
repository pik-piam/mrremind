#' @title calcEATLancetDiets
#' @description 
#' Calculates daily per capita intake for MAgPIE food commodities that are consistent with diet scenarios 
#' developed by the EAT-Lancet Commission on healthy diets from sustainable food systems. 
#' The unit is kcal/day per capita or wm/day per capita. 
#' Mapping of intake from EAT Lancet to MAgPIE food commodities is done indivudually for the different available units.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' 
#' @param attributes attributes of different food commodities (available: kcal and wm). 
#' @param calib if TRUE, total daily per capita intake for MAgPIE food commodities is calibrated to EAT Lancet total intake. 
#' @param FAOcountr if TRUE, estimates for countries not covered in FAOSTAT are set to Zero.
#' 
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{readEATLancet}},
#' \code{\link{convertEATLancet}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EATLancetDiets")
#' }
#' @export

calcEATLancetDiets <- function(attributes = c("wm","kcal"), calib = TRUE, FAOcountr = TRUE){
  
  #read data for EAT Lancet diet (intake)
  EAT_diets <- readSource(type="EATLancet", subtype="cons_data")
  getNames(EAT_diets,dim=2) <- c("wm","kcal")
  getSets(EAT_diets)[4] <- "unit"
  
  #read data on food supply based on FAOSTAT, aggregated to MAgPIE commodities
  #food supply includes householde waste: food supply = intake + waste
  kfo <- findset("kfo")
  fsupply.hist <- calcOutput(type = "FoodSupplyPast", aggregate = FALSE, per_capita = TRUE, product_aggr = FALSE, attributes = c("wm","kcal"))
  getSets(fsupply.hist)[3:4] <- c("kfo","unit")
  
  #define new diet object with MAgPIE food products
  years <- dimnames(EAT_diets)[[2]]
  iso <- dimnames(EAT_diets)[[1]]
  Mag_EAT_diets <- new.magpie(cells_and_regions = iso, years = years, 
                   names = getNames(EAT_diets,dim=1), sets=c("region","year","kcal_scn"))
  
  Mag_EAT_diets <- add_dimension(Mag_EAT_diets, add = "unit", dim = 3.2)
  Mag_EAT_diets <- add_columns(Mag_EAT_diets, addnm = getNames(EAT_diets,dim=2), dim = 3.2)
  Mag_EAT_diets <- Mag_EAT_diets[,,"dummy", pmatch=T, invert=T]
  
  Mag_EAT_diets <- add_dimension(Mag_EAT_diets, add = "diet_scn", dim = 3.3)
  Mag_EAT_diets <- add_columns(Mag_EAT_diets, addnm = getNames(EAT_diets,dim=3), dim = 3.3)
  Mag_EAT_diets <- Mag_EAT_diets[,,"dummy", pmatch=T, invert=T]
  
  Mag_EAT_diets <- add_dimension(Mag_EAT_diets, add = "kfo", dim = 3.4)
  Mag_EAT_diets <- add_columns(Mag_EAT_diets, addnm = kfo, dim = 3.4)
  Mag_EAT_diets <- Mag_EAT_diets[,,"dummy", pmatch=T, invert=T]
  
  
  ##### Mapping of EAT Lancet food commodities to MAgPIE commodities:
  
  
  #0. No information needed from EAT LAncet diets since food demand is zero:
  Mag_EAT_diets[,,"scp"] <- 0
  
  
  #1. Direct 1:1 mapping possible
  dir_kfo <- c("fish","livst_chick","livst_egg","livst_milk","livst_pig",
                      "maiz","puls_pro","rice_pro","soybean","sugar")
  dir_food_group <- c("fish","poultry","eggs","milk","pork",
                      "maize","legumes","rice","soybeans","sugar")
  dir_rel_matrix <- cbind(dir_kfo,dir_food_group)
  
  Mag_EAT_diets[,,dir_kfo]<-toolAggregate(EAT_diets[,,dir_food_group],rel = dir_rel_matrix,
                            dim = 3.4,from = "dir_food_group",to = "dir_kfo", partrel=FALSE)
  
  
  #2. MagPIE commodities can be represented by several EAT Lancet commodities
  Mag_EAT_diets[,,"livst_rum"] <- EAT_diets[,,"beef"] + EAT_diets[,,"lamb"]
  Mag_EAT_diets[,,"oils"] <- EAT_diets[,,"oil_palm"] + EAT_diets[,,"oil_veg"]
  
  
  #3. EAT Lancet commodities are an aggregate of several commodities in MAgPIE
  
  #roots
  roots <- c("cassav_sp","potato")
  for(i in roots){
    root.shr <- setYears(fsupply.hist[,"y2010",i]/dimSums(fsupply.hist[,"y2010",roots],dim = 3.1),NULL)
    if(any(!is.finite(root.shr)) ) {
      replacement <- as.magpie(apply(root.shr,3,mean, na.rm=TRUE))
      root.shr <- toolNAreplace(root.shr, replaceby=replacement)$x
    }
    Mag_EAT_diets[,,i] <- EAT_diets[,,"roots"]*root.shr
  } 
  
   
  #4. Definition mismatch between MAgPIE and EAT Lancet commodities 
  #(not possible to express commodity_dataset_x as sum(commodities_dataset_y))

  ###temperate and tropical cereals
  crls_Mag <- c("tece","trce")
  crls_EAT <- c("wheat","othr_grains")
  #estimate waste share for cereals based on 2010 food demand (FAO) and 2010 BMK intake from EAT Lancet
  waste_shr_crls <- 1-setYears(dimSums(EAT_diets[,,"BMK"][,,"2100kcal"][,"y2010",crls_EAT],dim=3.4)/dimSums(fsupply.hist[,"y2010",crls_Mag],dim=3.1),NULL)
  if(any(!is.finite(waste_shr_crls)) ) {
    temp_waste_shr <- waste_shr_crls
    temp_waste_shr[which(!is.finite(temp_waste_shr))] <- 0
    replacement <- as.magpie(apply(temp_waste_shr,3,mean, na.rm=TRUE))
    waste_shr_crls <- collapseNames(toolNAreplace(waste_shr_crls, replaceby=replacement)$x)
  }
  
  mult_factor_othgr <- collapseNames(EAT_diets[,,"othr_grains"]/setYears(EAT_diets[,"y2010","othr_grains"][,,"BMK"],NULL))
  if(any(!is.finite(mult_factor_othgr)) ) {
    temp_mult_factor <- mult_factor_othgr
    temp_mult_factor[which(!is.finite(temp_mult_factor))] <- 1
    replacement <- dimSums(mult_factor_othgr,dim=1)
    for(t in years){
      replacement[,t,] <- as.magpie(apply(temp_mult_factor[,t,],3,mean, na.rm=TRUE))
    }
    mult_factor_othgr <- collapseNames(toolNAreplace(mult_factor_othgr, replaceby=replacement)$x)
  }

  balance.post.crls <- collapseNames(EAT_diets[,,"othr_grains"] - (
                  setYears(collapseNames(fsupply.hist[,"y2010","trce"]),NULL)*(1-waste_shr_crls[,,])*mult_factor_othgr))
  
  balance.post.crls[which(balance.post.crls<0)]=0
  
  Mag_EAT_diets[,,"trce"] <- EAT_diets[,,"othr_grains"] - balance.post.crls
  Mag_EAT_diets[,,"tece"] <- EAT_diets[,,"wheat"] + balance.post.crls
  
  
  ###vegetables, fruits and nuts/seeds
  
  #the EAT Lancet categories "nuts_seeds","vegetables" and "fruits" are mapped 
  #to the MAgPIE categories "groundnut","rapeseed","sunflower" and "others"
  
  vfns_Mag <- c("others","groundnut","rapeseed","sunflower")
  ns_Mag <- c("groundnut","rapeseed","sunflower")
  vfns_EAT <- c("nuts_seeds","vegetables","fruits")
  
  #step 1: estimate "Mag_EAT_diets[,,"others"]" by using an intermediate mapping 
  #        between EAT Lancet (supercategory "vegfruits" and original "nuts_seeds") and MAgPIE ("others" and supercategory "nutsoilseeds")
  
  tmp_EAT_vegfruits <- EAT_diets[,,"vegetables"] + EAT_diets[,,"fruits"]
  tmp_EAT_nuts_seeds <- EAT_diets[,,"nuts_seeds"]

  #For the split of EAT Lancet "nuts_seeds" into MAgPIE "others" and "nutsoilseed", we use information on food availability of 
  #MAgPIE "others" and "nutsoilseed" and transform it into intake equivalents by using waste assumptions (from FAO) combined with 
  #conversion factors into edible matter that were both applied in Springmann et al. (2018) to calculate baseline food consumption 
  #from IMPACT food availability data, as explained in the Supplement to: 
  #Springmann M, Wiebe K, Mason-D’Croz D, Sulser T, Rayner M, Scarborough P. Health and nutritional aspects of sustainable diet strategies and their
  #association with environmental impacts: a global modelling analysis with country-level detail. Lancet Planet Health 2018  
  
  #Conversion factors into edible matter: 0.77 for fruits and vegetables, 1 for oilseeds and pulses
  conv_fact_vf <- 0.77
  FAO_waste_shr <- readSource(type="FAOLossesWaste",subtype="Consumption")
    
  #calibration of waste (vegetables, fruits, all nuts and seeds), as estimated following Springmann et al. 2019, with Mag_waste = food availability - intake 
  #based on information on 2010 food availability in MAgPIE (derived from FAOSTAT) and 2010 BMK intake from EAT Lancet
  
  Mag_waste_vfns <- collapseNames(dimSums(fsupply.hist[,"y2010",vfns_Mag],dim=3.1) - dimSums(EAT_diets[,,"BMK"][,,"2100kcal"][,"y2010",vfns_EAT],dim=3.4))
  
  waste_vfns_estimated <- collapseNames(FAO_waste_shr[,"y2010","Oilseeds and pulses"]*dimSums(fsupply.hist[,"y2010",ns_Mag],dim=3.1)
                           + FAO_waste_shr[,"y2010","Fruits and vegetables"]*fsupply.hist[,"y2010","others"] 
                           + (1-FAO_waste_shr[,"y2010","Fruits and vegetables"])*(1-conv_fact_vf)*fsupply.hist[,"y2010","others"] )
  
  calib_fact_waste <- Mag_waste_vfns/waste_vfns_estimated
  
  if(any(!is.finite(calib_fact_waste)) ){
    temp_calib <- calib_fact_waste
    temp_calib[which(!is.finite(temp_calib))] <- 1
    replacement <- as.magpie(apply(temp_calib,3,mean, na.rm=TRUE))
    calib_fact_waste <- collapseNames(toolNAreplace(calib_fact_waste, replaceby=replacement)$x)
  }
  
  waste_shr_ns <- collapseNames(setYears(calib_fact_waste*FAO_waste_shr[,"y2010","Oilseeds and pulses"],NULL))
  
  mult_factor_ns <- collapseNames(tmp_EAT_nuts_seeds/setYears(tmp_EAT_nuts_seeds[,"y2010","BMK"],NULL))
  if(any(!is.finite(mult_factor_ns)) ) {
    temp_mult_factor <- mult_factor_ns
    temp_mult_factor[which(!is.finite(temp_mult_factor))] <- 1
    replacement <- dimSums(mult_factor_ns,dim=1)
    for(t in years){
      replacement[,t,] <- as.magpie(apply(temp_mult_factor[,t,],3,mean, na.rm=TRUE))
    }
    mult_factor_ns <- collapseNames(toolNAreplace(mult_factor_ns, replaceby=replacement)$x)
  }
    
  balance.post.vfns <- collapseNames(tmp_EAT_nuts_seeds - (
    setYears(dimSums(fsupply.hist[,"y2010",ns_Mag],dim=3.1),NULL)*(1-waste_shr_ns[,,])*mult_factor_ns))
  
  balance.post.vfns[which(balance.post.vfns<0)]=0
  
  tmp_Mag_nutsoilseeds      <- tmp_EAT_nuts_seeds - balance.post.vfns
  Mag_EAT_diets[,,"others"] <- tmp_EAT_vegfruits + balance.post.vfns
  
 
  #step 2: now, the intermediate MagPIE supercategory "nutsoilseeds" can be distributed to its components ("groundnut","rapeseed","sunflower"), 
  #        similar to the case of roots above
    
  ns_Mag <- c("groundnut","rapeseed","sunflower")
    
  for(i in ns_Mag){
    nutsoilseeds.shr <- setYears(fsupply.hist[,"y2010",i]/dimSums(fsupply.hist[,"y2010",ns_Mag],dim = 3.1),NULL)
    if(any(!is.finite(nutsoilseeds.shr)) ) {
      replacement <- as.magpie(apply(nutsoilseeds.shr,3,mean, na.rm=TRUE))
      nutsoilseeds.shr <- toolNAreplace(nutsoilseeds.shr, replaceby=replacement)$x
    }
    Mag_EAT_diets[,,i] <- tmp_Mag_nutsoilseeds*nutsoilseeds.shr
  } 


  #5. No quantitative information about future contribution of certain foods to diets in EAT Lancet dataset
  
  ### sugar crop products besides sugar:
  #for the BMK scenario, estimate intake of sugar crop products based on food supply data, but only until the value 
  #from the EAT Lancet "othrcrp" category is reached
  Mag_sugrcrp <- c("molasses","sugr_beet","sugr_cane")
  Mag_EAT_diets[,,Mag_sugrcrp] <- 0
  
  mult_factor_sc <- collapseNames(EAT_diets[,,"BMK"][,,"othrcrp"]
                                  /setYears(EAT_diets[,"y2010","BMK"][,,"2100kcal"][,,"othrcrp"],NULL))
  if(any(!is.finite(mult_factor_sc)) ) {
    temp_mult_factor <- mult_factor_sc
    temp_mult_factor[which(!is.finite(temp_mult_factor))] <- 1
    replacement <- dimSums(mult_factor_sc,dim=1)
    for(t in years){
      replacement[,t,] <- as.magpie(apply(temp_mult_factor[,t,],3,mean, na.rm=TRUE))
    }
    mult_factor_sc <- collapseNames(toolNAreplace(mult_factor_sc, replaceby=replacement)$x)
  }
  
  for(t in years){
    Mag_EAT_diets[,t,Mag_sugrcrp][,,"BMK"] <- setYears(fsupply.hist[,"y2010",Mag_sugrcrp]*(1-FAO_waste_shr[,"y2010","Fruits and vegetables"])
                                              *conv_fact_vf,t)*mult_factor_sc[,t,]
  
    sugr_gap_reg <- where(dimSums(Mag_EAT_diets[,t,Mag_sugrcrp],dim=3.4)>EAT_diets[,t,"BMK"][,,"othrcrp"])$true$regions
    sugr_gap_cal <- collapseNames(EAT_diets[sugr_gap_reg,t,"BMK"][,,"2100kcal"][,,"othrcrp"]/dimSums(Mag_EAT_diets[sugr_gap_reg,t,Mag_sugrcrp][,,"BMK"][,,"2100kcal"],dim=3.4))
    sugr_gap_cal[which(sugr_gap_cal>1)] <- 1
    
    Mag_EAT_diets[sugr_gap_reg,t,Mag_sugrcrp][,,"BMK"] <- collapseNames(Mag_EAT_diets[sugr_gap_reg,t,Mag_sugrcrp][,,"BMK"]*sugr_gap_cal[sugr_gap_reg,t,])
  }
  
  #estimate the residual amount of "othrcrp" without the amount that is mapped to sugar crops
  res_EAT_othrcrp <- EAT_diets[,,"othrcrp"] - dimSums(Mag_EAT_diets[,,Mag_sugrcrp],dim=3.4)
  #mean: 4.3 kcal; min: 0 kcal; max: 231.6 kcal
  
  
  ### alcohol:
  #for the BMK scenario, estimate intake of alcohol based on food supply data, but only until the value 
  #from the residual EAT Lancet category "othrcrp" plus the EAT Lancet category "othrcal" is reached
  
  Mag_EAT_diets[,,"alcohol"] <- 0
  
  tmp_EAT_othrcal_pos <- EAT_diets[,,"othrcal"]
  tmp_EAT_othrcal_pos[which(tmp_EAT_othrcal_pos<0)]=0
  tmp_EAT_othrcal_neg <- EAT_diets[,,"othrcal"]
  tmp_EAT_othrcal_neg[which(tmp_EAT_othrcal_neg>0)]=0
  
  balance_post_al <- collapseNames(tmp_EAT_othrcal_pos[,,] + res_EAT_othrcrp)
  
  mult_factor_al <- collapseNames(balance_post_al[,,"BMK"]
                                  /setYears(balance_post_al[,"y2010","BMK"][,,"2100kcal"],NULL))
  if(any(!is.finite(mult_factor_al)) ) {
    temp_mult_factor <- mult_factor_al
    temp_mult_factor[which(!is.finite(temp_mult_factor))] <- 1
    replacement <- dimSums(mult_factor_al,dim=1)
    for(t in years){
      replacement[,t,] <- as.magpie(apply(temp_mult_factor[,t,],3,mean, na.rm=TRUE))
    }
    mult_factor_al <- collapseNames(toolNAreplace(mult_factor_al, replaceby=replacement)$x)
  }
  
  for(t in years){
    Mag_EAT_diets[,t,"alcohol"][,,"BMK"] <- setYears(fsupply.hist[,"y2010","alcohol"],t)*0.90*mult_factor_sc[,t,]
    
    for(a in getNames(Mag_EAT_diets,dim=2)){
      alcohol_gap_reg <- where(Mag_EAT_diets[,t,"alcohol"][,,a] > balance_post_al[,t,"BMK"][,,a])$true$regions
      alcohol_gap_cal <- collapseNames(balance_post_al[alcohol_gap_reg,t,"BMK"][,,"2100kcal"][,,a]/dimSums(Mag_EAT_diets[alcohol_gap_reg,t,"alcohol"][,,"BMK"][,,"2100kcal"][,,a],dim=3.4))
      alcohol_gap_cal[which(alcohol_gap_cal>1)] <- 1
    
      Mag_EAT_diets[alcohol_gap_reg,t,"alcohol"][,,"BMK"][,,a] <- collapseNames(Mag_EAT_diets[alcohol_gap_reg,t,"alcohol"][,,"BMK"][,,a]*alcohol_gap_cal[alcohol_gap_reg,t,])
    }
  }
  
  
  ### brans: 
  # currently, intake of brans is set to Zero, since there is no explicit information available in the EAT Lancet data set
  Mag_EAT_diets[,,"brans"] <- 0
  
  #Ideas for further improvement:
  #in EAT Lancet diet, cereals should be consumed in the form of wholegrain meals
  #this can be realized re-allocating a share of calories from cereles to brans according to the milling share
  
  
  ##### Mapping of EAT Lancet food commodities to MAgPIE commodities finished ####
  ################################################################################
  
  
  
  ##### Intermediate check if distribution from EAT Lancet food commodities (disregarding "othrcrp"and "othrcal") 
  #to MAgPIE commodities is accurate
  
  EAT_Lancet_intake_no_balancing <- dimSums(EAT_diets[,,c("othrcrp","othrcal"),invert=T],dim=3.4)
  Mag_EAT_intake_no_balancing <- dimSums(Mag_EAT_diets[,,Mag_sugrcrp,invert=T][,,"alcohol",invert=T],dim=3.4)
  check_intake_no_balancing <- EAT_Lancet_intake_no_balancing - Mag_EAT_intake_no_balancing
 
  if(max(abs(check_intake_no_balancing))>0.000000001){
    warning("Distribution of EAT Lancet diet categories to MAgPIE categories below required accuracy")
  }

  
  ##### Calibration of mapped MAgPIE diets to total EAT Lancet Intake
  
  data.out <- Mag_EAT_diets
  
  if(calib){
    EAT_Lancet_intake <- dimSums(EAT_diets,dim=3.4)
    Mag_EAT_intake_total <- dimSums(Mag_EAT_diets,dim=3.4)
    
    intake_calib_factor <- EAT_Lancet_intake/Mag_EAT_intake_total
    Mag_EAT_diets_calib <- intake_calib_factor*Mag_EAT_diets
    
    data.out <- Mag_EAT_diets_calib
  }
  
 
  #### Set values for all counries to ZERO where no FAO statistics exist, e.i. where fsupply.hist is ZERO
  
  if(FAOcountr){
    nonFAOSTAT <- where(dimSums(fsupply.hist[,"y2010","kcal"],dim = 3.1)==0)$true$regions
    data.out[nonFAOSTAT,,]=0 
  }
  
  
  #### Select nutrition attributes for which data should be returned
  
  data.out <- collapseNames(data.out[,,attributes])

  
  #### Define weights and units
  
  weight.pop <- collapseNames(calcOutput("Population",aggregate = FALSE)[,years,"pop_SSP2"])
  unit <- "kcal or wm per capita per day"
  
  return(list(x=data.out,
              weight=weight.pop,
              unit=unit,
              description="Daily per capita food intake for MAgPIE commodities consistent with EAT Lancet diet scenarios")
  )
}
