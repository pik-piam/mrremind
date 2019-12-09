#' calcGridPop
#' 
#' @description Past and future (SSP1-5) population based on HYDE3.2 and Jones & O'Neill (2016)
#' Data is scaled to match WDI data from calcPopulation
#' NOTE that some scaling factors for the future (for small countries...Gambia and Djibouti) are somewhat off, data read in is 50% of WDI data, most likely due to large resolution
#' @param subtype past (1965-2005), future (2005-2010) or all (divergence changed to start at 2015)
#' @param cellular only cellular
#' @return Population in millions.
#' @author David Chen
#' @importFrom magclass add_columns collapseNames
#' @importFrom magpiesets findset
#' @importFrom madrat calcOutput toolGetMapping toolAggregate


calcGridPop <- function(subtype="all", cellular=T) {
  if(!cellular)(stop("Run calcPopulation instead"))
  ##past 
  if (subtype=="past"){
  gridpop <- readSource("GridPop",subtype="past",convert=F)
  CountryToCell     <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
  agg   <- toolAggregate(gridpop, rel=CountryToCell, from="celliso", to="iso", partrel=TRUE)
  
  ## scale to match WDI country-level pop 
  
  pop <- calcOutput("Population",aggregate=F)
  past <- findset("past")
  past <- past[-10]
  agg <-  agg[,past,]
  
  #scaling factor sc_f applied to every cell
  sc_f <- (agg/1e6)/pop[getRegions(agg),past,"pop_SSP2"]
  
  gridpop1 <- gridpop[,past,] / sc_f[getRegions(gridpop),,]
  
  #mauritius and ATF have 0, but the cells exist, so get filled. 
  #the single MUS cell gets the pop, the ATF cells get even division of pop
  gridpop1["MUS",,] <- 1e6*pop["MUS",past,"pop_SSP2"]
  gridpop1["ATF",,] <- 1e6*pop["ATF",past,"pop_SSP2"]/length(getCells(gridpop["ATF",,]))
  gridpop1["SGS",,] <- 1e6*pop["SGS",past,"pop_SSP2"]/length(getCells(gridpop["SGS",,]))
  x <- collapseNames(gridpop1,collapsedim=2)
  x <- add_columns(x, dim=3.1,addnm=c(getNames(pop)[1:5]))
  x[,,2:6] <- x[,,1]
  x <- (x[,,-1])/1e6
}
  
  ##future
  if (subtype=="future"){
  gridpop <- readSource("GridPop",subtype="future",convert=F)
  CountryToCell     <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
  agg   <- toolAggregate(gridpop, rel=CountryToCell, from="celliso", to="iso", partrel=TRUE)
  
  ## scale to match moinput country-level pop 
  pop <- calcOutput("Population",aggregate=F)[,,1:5]
  future <- getYears(agg)
  
  #scaling factor sc_f applied to every cell
  sc_f <- (agg/1e6)/pop[getRegions(agg),future,]
  gridpop1 <- gridpop/sc_f[getRegions(gridpop),,]
  
  #ATF, SJM, SGS have 0, but the cells exist, so get filled. 
  #the few cells for each island get even division of total pop
  for (i in c("SGS","ATF","SJM")){
  gridpop1[i,,] <- 1e6*pop[i,future,]/length(getCells(gridpop[i,,]))
  }
  x <- gridpop1/1e6
}
  if (subtype=="all"){
    past <- calcOutput("GridPop",subtype="past", aggregate=F)
    future <- calcOutput("GridPop", subtype="future", aggregate=F)
    x <- mbind(past,future)
  }
  return(list(x=x,
              weight=NULL,
              unit="million",
              description="Population in millions",
              isocountries=FALSE))
}   