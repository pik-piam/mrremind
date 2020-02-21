#' @importFrom magclass setNames


calcCollectFoodDemandRegressionData<-function()
{
  pop <- setNames(readSource("WDI",subtype="SP.POP.TOTL",convert = F),"pop")
  urban <- setNames(readSource("WDI",subtype="SP.URB.TOTL.IN.ZS",convert = F)/100,"urban")
  gdp_pc <- readSource("James",subtype="IHME_USD05_PPP_pc",convert = F)
  food_supply_crop <- readSource("FAO",subtype="FSCrop",convert = F)
  food_supply_live <- readSource("FAO",subtype="FSLive",convert = F)
  food_supply <- toolFAOcombine(food_supply_crop,food_supply_live, combine="Item")
  Lutz <- readSource("Lutz2014",convert=FALSE)
  CZ <- readSource("Koeppen",convert=FALSE) #klimazone
  
### aggregate food groups ###
  relationmatrix <- toolGetMapping("FAOitems.csv", type = "sectoral", where="mappingfolder")
  relationmatrix <- relationmatrix[,which(names(relationmatrix)%in%c("FoodBalanceItem","k"))]
  relationmatrix <- relationmatrix[-which(duplicated(relationmatrix[,1])==T),]
  
  food_supply <- collapseNames(food_supply[,,"food_supply_kcal/cap/day"])
  kcal <- toolAggregate(#x = kcal,
                          x = food_supply,
                          rel =relationmatrix,
                          dim = 3.1,
                          from = "FoodBalanceItem",
                          to = "k", 
                          partrel=TRUE)
  
  kcal<-add_columns(kcal,addnm = c("brans","scp"))
  kcal[,,c("brans","scp")]<-0
  
  kcal <- kcal[,,"remaining",invert=TRUE]

  
  missing <- dimSums(kcal,dim=3,na.rm=TRUE) # missing values
  missing[missing == 0] <- NA
  missing[!is.na(missing)]<-1
  kcal[is.na(kcal)] = 0
  kcal = kcal * missing
  
### Lutz ###
  LutzSSP2 <- time_interpolate(Lutz[,,"SSP2"],
                                paste0("y", 1970:2011),
                                integrate_interpolated_years=F,
                                extrapolation_type = "linear")
  TotalBoth <- LutzSSP2[,,"Total"][,,"Both"][,,"All"]
  TotalFem <- LutzSSP2[,,"Total"][,,"Female"][,,"All"]
  
  ### gender ###
  femShare <- mbind(setNames(
    dimSums(TotalFem/TotalBoth, dim=3),
    "femaleShare"))
  names(dimnames(femShare)) <- names(dimnames(gdp_pc))

  ### education ###  
  education<-mbind(
    setNames(dimSums(LutzSSP2[,,"Post Secondary"][,,"Both"][,,"All"] 
                     / TotalBoth,
                     dim=3),
             "college"),
    setNames(dimSums(LutzSSP2[,,"Post Secondary"][,,"Female"][,,"All"]
                     / TotalFem,
                     dim=3),
             "femcollege"),
    setNames(dimSums(LutzSSP2[,,c("Incomplete Primary",
                                   "No Education",
                                   "Primary")][,,"Both"][,,"All"] 
                     / TotalBoth,
                     dim=3),
             "low_education")
  )
  #education[,,2] <- education[,,2] - education[,,1]
  names(dimnames(education)) <- names(dimnames(gdp_pc))
  if(any(education<0, na.rm=TRUE))
  {
    stop("education share smaller 0")
  }
  if(any(education>1, na.rm=TRUE))
  {
    stop("education share larger 1")
  }

  ### age ###
  age<-mbind(
    setNames(dimSums(LutzSSP2[,,"Total"][,,"Both"][,,c(
      "0--4","5--9","10--14")] 
                     / TotalBoth,
                     dim=3),
             "below15"),
    setNames(dimSums(LutzSSP2[,,"Total"][,,"Both"][,,c(
      "15--19","20--24","25--29","30--34","35--39",
      "40--44","45--49","50--54","55--59","60--64")] 
                     / TotalBoth,
                     dim=3),
             "15-64"),
    setNames(dimSums(LutzSSP2[,,"Total"][,,"Both"][,,c(
      "65--69","70--74","75--79","80--84",
      "85--89","90--94","95--99","100+")] 
                     / TotalBoth,
                     dim=3),
             "above64")
  )
  names(dimnames(age)) <- names(dimnames(gdp_pc))
  
### urbanization and population ###
  getCells(pop) <- countrycode(getCells(pop),"iso2c","iso3c")
  getCells(urban) <- countrycode(getCells(urban),"iso2c","iso3c")
  pop <- pop[which(!is.na(dimnames(pop)[[1]])),,]
  urban <- urban[which(!is.na(dimnames(urban)[[1]])),,]
  
### GDP ###
  gdp_pc <- gdp_pc[,,"IHME_USD05_PPP_pc"]
  getNames(gdp_pc) <- "IHME_USD05_PPP"
  
### bind datasets together for same years and regions ###
  years <- intersect(getYears(kcal),
                     intersect(getYears(gdp_pc),
                               intersect(getYears(education),
                                         intersect(getYears(femShare),
                                                   intersect(getYears(age),
                                                             intersect(getYears(pop),
                                                                       getYears(urban)
                                         ))))))
  
  countries <- intersect(getRegions(kcal),
                         intersect(getRegions(gdp_pc),
                                   intersect(getRegions(education),
                                             intersect(getRegions(femShare),
                                                       intersect(getRegions(age),
                                                                 intersect(getRegions(pop),
                                                                           getRegions(urban)
                                             ))))))
  
  ### Climate zone ###
  CZ <- dimSums(CZ[,,c("kg_p_af","kg_p_aw","kg_p_bs","kg_p_cf","kg_p_df","kg_p_e")],dim=3)
  countries <- intersect(countries,getRegions(CZ))
  CZ_magpie <- new.magpie(cells_and_regions = countries, years = years)
  for(i in years) CZ_magpie[,i,] <- CZ[countries,,]
  CZ_magpie <- setNames(CZ_magpie,"climate")
  
### out ###
  out <- mbind(gdp_pc[countries,years,],
               femShare[countries,years,],
               education[countries,years,],
               age[countries,years,],
               pop[countries,years,],
               urban[countries,years,],
               CZ_magpie[countries,years,]
               ,kcal[countries,years,]
               )
  
  pop <- setNames(out[,,"pop"],NULL)
  out <- out * pop
  out[,,"pop"] <- pop

  return(list(x = out,
              weight = NULL,
              unit = "for population in mio people, 
                      for GDP in Mio USD05 PPP, 
                      education in mio and urbanization in mio people, 
                      food demand in mio kcal per day",
              description = "Merged dataset containing raw data for regression",
              min = 0,
              isocountries = FALSE)
  )
} 