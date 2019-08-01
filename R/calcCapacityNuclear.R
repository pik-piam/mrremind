
calcCapacityNuclear <- function() {
 ### overall philosophy:
  ## currently under construction goes online over the 5-year long 2020 timestep
  ## 90% of planned and 60% of proposed plants can come online in 2025 or 2030 timestep
  ## this corresponds well with maximum to be expected capacities for India and China
  ## China target 120-150 GW in 2030 (https://af.reuters.com/article/africaTech/idAFL3N16M3QX)
  ## India 63 GW in 2032 (though pre-Fukushima) (https://economictimes.indiatimes.com/industry/energy/power/india-eyeing-63000-mw-nuclear-power-capacity-by-2032-npcil/articleshow/6730724.cms)
  
  #additional assumption: gross-net losses vary between 3 (Shin-Kori Unit 3)- 12 (Fuqing Unit 5) %, so 5% seems good assumption: 
  grossnet = 1.05
  x <- readSource("IAEA")
  
  out <- new.magpie(getRegions(x),c(2015,2020,2025,2030),"tnrs")
  # allocate data and convert from MW into TW
  #total capacity in 2015: snapshot of operable reactors in early 2016
  out[,2015,] <- setYears(x[,2016,"REACTORS OPERABLE (MWe net)"] / 1000000,2015)
  #total capacity addition for next 5 years: under construction + plants that came online between early 2016 and August 2018
  out[,2020,] <- (  setYears(x[,2018,"REACTORS UNDER CONSTRUCTION (MWe gross)"]/grossnet,2020) 
                  + setYears(x[,2018,"REACTORS OPERABLE (MWe net)"],NULL)
                  - setYears(x[,2016,"REACTORS OPERABLE (MWe net)"],NULL)) / 1000000
  #maximum capacity addition for 5 years in 2025 period: 50% of planned and 30% of proposed, + 10% of 2015 to represent extensions
  out[,2025,] <- (
                0.5*setYears(x[,2018,"REACTORS PLANNED (MWe gross)"]/grossnet,2025)
              + 0.3*setYears(x[,2018,"REACTORS PROPOSED (MWe gross)"]/grossnet,NULL)
              + 0.1*setYears(x[,2016,"REACTORS OPERABLE (MWe net)"],NULL)) / 1000000
  #maximum capacity addition for 5 years in 2025 period: 50% of planned and 70% of proposed, + 10% of 2015 to represent extensions
  out[,2030,] <- (
                0.5*setYears(x[,2018,"REACTORS PLANNED (MWe gross)"]/grossnet,2030)
              + 0.7*setYears(x[,2018,"REACTORS PROPOSED (MWe gross)"]/grossnet,NULL)
              + 0.1*setYears(x[,2016,"REACTORS OPERABLE (MWe net)"],NULL)) / 1000000

  #special treatment for countries in the list (considering last political decisions)
  out["VNM",2025,] <- 0 #Vietnam has paused the nuclear program, so first operation definitely not earlier
                        #than 2028 http://www.world-nuclear.org/information-library/country-profiles/countries-t-z/vietnam.aspx
  #special treatment to avoid infeasibility and open nuclear potential for Africa
  #all countries not in the list of proposed plants, but rated as "Developing plans" can have a 500MW in 2025 and 2GW in 2030 periods
  # http://www.world-nuclear.org/information-library/country-profiles/others/emerging-nuclear-energy-countries.aspx
  # Israel, Nigeria, Kenya, Laos, Malaysia, Morocco, Algeria
  out["ISR",2025,] <- 0.0005
  out["NGA",2025,] <- 0.0005
  out["KEN",2025,] <- 0.0005
  out["LAO",2025,] <- 0.0005
  out["MYS",2025,] <- 0.0005
  out["MAR",2025,] <- 0.0005
  out["DZA",2025,] <- 0.0005
  out["ISR",2030,] <- 0.002
  out["NGA",2030,] <- 0.002
  out["KEN",2030,] <- 0.002
  out["LAO",2030,] <- 0.002
  out["MYS",2030,] <- 0.002
  out["MAR",2030,] <- 0.002
  out["DZA",2030,] <- 0.002
  return(list(x      = out,
              weight = NULL,
              unit        = "TW",
              description = "capacity of operating nuclear plants in 2015, fixed additions for 5-year period 2020, and upper limits on additions for 5-year periods 2025 and 2030"))
  
}
