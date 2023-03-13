#' FE Share parameters used in REMIND
#' 
#' @param subtype 'ind_coal' for the share of coal used in industry. 'ind_bio' for the share of biomass used in industry
#' 
#' @author Antoine Levesque


calcFEShares <- function(subtype) {
  
   #---- READ Data -----
  
  edge_buildings = calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = F)
  output = calcOutput("IO",subtype  = "output", aggregate = F)
  fe_demand = calcOutput("FEdemand", aggregate = F)[,,"gdp_SSP1"]
  fe_demand = collapseNames(fe_demand)
  #---- Process Data ----

  if (subtype == "ind_coal"){
    share = 1 - dimSums(edge_buildings[,2005,"coal"])/output[,2005,"pecoal.sesofos.coaltr"]
    weight = output[,2005,"pecoal.sesofos.coaltr"]
    descr = "share of coal used in industry (computed by retrieving buildings uses, i.e., considering transport and ONONSPEC are null)"
  } else if (subtype == "ind_bio"){
    share = 1 - dimSums(edge_buildings[,2005,c("biomod","biotrad")])/dimSums(output[,2005,c("pebiolc.sesobio.biotr", "pebiolc.sesobio.biotrmod")], dim = 3)
    weight = dimSums(output[,2005,c("pebiolc.sesobio.biotr", "pebiolc.sesobio.biotrmod")], dim = 3)
    descr = "share of biomass used in industry (computed by retrieving buildings uses, i.e., considering transport and ONONSPEC are null)"
  } else if (subtype == "ind_liq"){
    share = fe_demand[,2005,c("fehoi")] / (fe_demand[,2005,c("fehoi")] + fe_demand[,2005,c("fehob")])
    weight = (fe_demand[,2005,c("fehoi")] + fe_demand[,2005,c("fehob")])
    descr = "share of stationary heating oil used in industry"
  }
  
  share = collapseNames(share)
  share[is.na(share)] = 0
  weight[is.na(weight)] = 0
   return(list(x= share,weight=weight,
              unit = "dimensionless",
              description = descr))
}
