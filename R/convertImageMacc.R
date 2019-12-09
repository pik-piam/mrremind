#' Convert subtypes of the ImageMacc data
#' 
#' Convert subtypes from ImageMacc to data on ISO country level. Correct values
#' for N2O of the subtype "baseline_sources" from N to N2O (factor: 44/28.
#' 
#' 
#' @param x MAgPIE object containing ImageMacc data mixed on region level
#' @param subtype data subtype. Either CH4_Energy_Industry", "CH4_Landuse",
#' "N2O_Energy_Industry", "N2O_Landuse", "HFC_tot", "SF6_tot", "PFC_tot" or
#' "baseline_sources"
#' @return ImageMacc data as MAgPIE object for all subtypes aggregated to
#' country level
#' @author Nele Steinmetz
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{
#' a <- readSource("ImageMacc","CH4_Energy_Industry")
#' a <- readSource("ImageMacc","CH4_Landuse")
#' a <- readSource("ImageMacc","N2O_Energy_Industry")
#' a <- readSource("ImageMacc","N2O_Landuse")
#' a <- readSource("ImageMacc","HFC_tot")
#' a <- readSource("ImageMacc","SF6_tot")
#' a <- readSource("ImageMacc","PFC_tot")
#' a <- readSource("ImageMacc","baseline_sources")
#' }
#' 
convertImageMacc <- function(x,subtype) {
   map <- "regionmappingImageMacc.csv"
  
  if(subtype=="CH4_Energy_Industry"){
    y <- toolAggregate(x,map)
  }
  if(subtype=="CH4_Landuse") {
    y <- toolAggregate(x,map)
  }
  if(subtype=="N2O_Energy_Industry"){
    y <- toolAggregate(x,map)
  }
  if(subtype=="N2O_Landuse"){
    y <- toolAggregate(x,map)
  }
  if(subtype=="HFC_tot"){
    y <- toolAggregate(x,map)
  }
  if(subtype=="SF6_tot"){
    y <- toolAggregate(x,map)
  }
  if(subtype=="PFC_tot"){
    y <- toolAggregate(x,map)
  }
  else if(subtype=="baseline_sources"){
    # values for N2O have to be corrected by the factor 44/28 (N -> N2O)
    x[,,"N2O Transport"] <- x[,,"N2O Transport"]*(44/28)
    x[,,"N2O Adipic acid production"] <- x[,,"N2O Adipic acid production"]*(44/28)
    x[,,"N2O Nitric acid production"] <- x[,,"N2O Nitric acid production"]*(44/28)
    x[,,"N2O Fertilizer"] <- x[,,"N2O Fertilizer"]*(44/28)
    x[,,"N2O Animal waste"] <- x[,,"N2O Animal waste"]*(44/28)
    x[,,"N2O Domestic sewage"] <- x[,,"N2O Domestic sewage"]*(44/28)
    # weight
    CEDS_CH4 <- readSource("CEDS",subtype="CH4")[,2015,]
    LU_MagPie <- calcOutput("MacBaseLandUse",subtype="MAgPIE",aggregate=F)[,2015,]
    emiMac <- calcOutput("EmiMac",aggregate=F)
    FGases <- readSource("IMAGE")[,2010,]
    w <- toolCountryFill(new.magpie(cells_and_regions = NULL, years = NULL, names = c("CH4 coal losses/leakages","CH4 oil losses/leakages","CH4 natural gas losses/leakages","CH4 Landfills","CH4 Domestic Sewage","CH4 Wetland rice","CH4 Animals","CH4 Animal waste","N2O Transport","N2O Adipic acid production","N2O Nitric acid production","N2O Fertilizer","N2O Animal waste","N2O Domestic sewage","HFC","PFC","SF6")))
    w[,,"CH4 coal losses/leakages"]        <- CEDS_CH4[,,"1B1_Fugitive-solid-fuels"]
    w[,,"CH4 oil losses/leakages"]         <- CEDS_CH4[,,"1B2_Fugitive-petr-and-gas"]
    w[,,"CH4 natural gas losses/leakages"] <- CEDS_CH4[,,"1B2_Fugitive-petr-and-gas"]
    w[,,"CH4 Landfills"]                   <- emiMac[,,"ch4wstl"]
    w[,,"CH4 Domestic Sewage"]             <- emiMac[,,"ch4wsts"]
    w[,,"CH4 Wetland rice"]                <- dimReduce(LU_MagPie[,,"ch4rice.SSP2.rcp26"])
    w[,,"CH4 Animals"]                     <- dimReduce(LU_MagPie[,,"ch4animals.SSP2.rcp26"])
    w[,,"CH4 Animal waste"]                <- dimReduce(LU_MagPie[,,"ch4anmlwst.SSP2.rcp26"])
    w[,,"N2O Transport"]                   <- emiMac[,,"n2otrans"]
    w[,,"N2O Adipic acid production"]      <- emiMac[,,"n2oacid"]
    w[,,"N2O Nitric acid production"]      <- emiMac[,,"n2oacid"]
    w[,,"N2O Fertilizer"]                  <- dimSums(LU_MagPie[,,c("n2ofertin.SSP2.rcp26", "n2ofertcr.SSP2.rcp26", "n2ofertsom.SSP2.rcp26")])
    w[,,"N2O Animal waste"]                <- dimSums(LU_MagPie[,,c("n2oanwstc.SSP2.rcp26", "n2oanwstm.SSP2.rcp26", "n2oanwstp.SSP2.rcp26")])
    w[,,"N2O Domestic sewage"]             <- emiMac[,,"n2owaste"]
    w[,,"HFC"]                             <- dimReduce(FGases[,,"SSP2-26-SPA0-V13.Emissions|HFC.kt HFC134a-equiv/yr"])
    w[,,"PFC"]                             <- dimReduce(FGases[,,"SSP2-26-SPA0-V13.Emissions|PFC.kt CF4-equiv/yr"])
    w[,,"SF6"]                             <- dimReduce(FGases[,,"SSP2-26-SPA0-V13.Emissions|SF6.kt SF6/yr"])
    
    y <- toolAggregate(x,map,w)
    
  }

  return(y)
}
