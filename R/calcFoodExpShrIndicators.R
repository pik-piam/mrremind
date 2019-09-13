#' calcFoodExpShrIndicators
#' 
#' Returns historical food expenditure share calculation, for LCU, PPP and MER units
#' @param indicator MER PPP or LCU units
#' @author David Chen, Benjamin Bodirsky

calcFoodExpShrIndicators <- function(indicator = "PPP"){

# LCU food prices
  if (indicator=="LCU"){
  # prices <- readSource("FAO", "PricesProducerAnnualLCU", convert=FALSE)
    prices <- calcOutput("FillFoodPrice", indicator="LCU", aggregate=F)
}
#MER PPP food prices
if (indicator=="PPP" || indicator=="MER"){
  # prices <- readSource("FAO", "PricesProducerAnnual", convert=FALSE)
   prices <- calcOutput("FillFoodPrice", indicator="MER", aggregate=F)
}

# food supply
FS <- dimSums(calcOutput("FAOmassbalance",aggregate = FALSE)[,,"wm"][,,c("food","flour1")],dim=3.2)
FS <- collapseNames(FS)
FS <- FS*10^6 #convert Mt to tonnes
FS <- time_interpolate(FS, interpolated_year=getYears(prices), extrapolation_type = "linear")

#multiply prices by food supply
years <- intersect(getYears(FS), getYears(prices))
products <- intersect(getNames(FS), getNames(prices))
food_exp <- FS[,years,products] * prices[,years,products] 

#divide food expenditures by population
pop <- calcOutput("Population", aggregate=FALSE)[,,"pop_SSP2"]
pop<- time_interpolate(pop, interpolated_year= getYears(food_exp) , extrapolation_type = "linear")
pop <- collapseNames(pop)
pop <- pop*10^6 #million ppl 
food_exp <- food_exp/pop #food expenditure per capita
food_exp <- dimSums(food_exp, na.rm=T)


if (indicator=="LCU"){
  #gdp in current LCU from WDI,  
 gdppc_lcu <- readSource("WDI", subtype="NY.GDP.PCAP.CN")[,1990:2012,]
  regions <- intersect(getRegions(gdppc_lcu), getRegions(food_exp))
  food_exp_shr <- food_exp[regions,,]/gdppc_lcu[regions,,]
  food_exp_shr <- add_dimension(food_exp_shr, dim=3.1, add="scenario", nm="LCU")
}

if (indicator == "PPP") {
#gdp current PPP
  gdp_ppp <-  readSource("WDI", subtype = "NY.GDP.PCAP.PP.CD")[,1990:2012,]
  regions_ppp <- intersect(getRegions(food_exp), getRegions(gdp_ppp))
  food_exp_shr <- food_exp[regions_ppp,,]/gdp_ppp[regions_ppp,,]
  food_exp_shr <- add_dimension(food_exp_shr, dim=3.1, add="scenario", nm="PPP")
}

if (indicator == "MER") {
#gdp current MER
  gdp_MER <-  readSource("WDI", subtype="NY.GDP.PCAP.CD")[,1990:2012,]
  regions_mer <- intersect(getRegions(gdp_MER), getRegions(food_exp))
  food_exp_shr <- food_exp[regions_mer,,]/gdp_MER[regions_mer,,]
food_exp_shr <- add_dimension(food_exp_shr, dim=3.1, add="scenario", nm="MER")
}

food_exp_shr <- collapseNames(food_exp_shr)
getNames(food_exp_shr)<- "Household Expenditure|Food|Food Expenditure Share (USD/USD)"

return(list(x=food_exp_shr,
            weight=pop,
            unit="share",
            description="Food expenditure share based on MER PPP and LCU measurements of GDP"
            ))
}



# plotcountrymap(food_exp_shr[,2010,],cat=c(0, 0.01,0.05, 0.1,0.2,0.3,0.4,0.5,0.6),
#               mapTitle="Food Expenditure Share (LCU)",
#               colourPalette = c("#ffffd9","#edf8b1","#c7e9b4","#7fcdbb",
#                                "#41b6c4","#1d91c0","#225ea8","#0c2c84"))



