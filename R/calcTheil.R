#' Calculate regional Theil-T index
#' 

#' To calculate the regional Theil-T index (= correction to welfare function for a lognormal income distribution) we do the following:
#' (1) convert country-level Gini coefficients to Theil (2) calculate contribution to Theil-T index that includes both between-countries and within-country inequality 
#' (see e.g. https://en.wikipedia.org/wiki/Theil_index). The latter can then be aggregated with calcOutput().
#' 
#' NB 1: the aggregation depends on the region mapping. It is implemented such that the regionmapping specified in getConfig()$regionmapping is used.
#' 
#' NB 2: the result of calcOutput('Theil', aggregate = FALSE), is NOT the country Theil-T, but the unweighted contribution from a given country to the regional value.
#' 
#' @return magpie objects of unweighted contribution to Theil, weights (= country shares of regional GDP), docstring
#' @author Bjoern Soergel
#' @seealso \code{\link{calcOutput}} \code{\link{convertGini},\link{readGini}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput("Theil")
#' }
#' 
#' @importFrom assertthat assert_that
#' @importFrom stats qnorm

calcTheil <- function(){
  
  #Gini & Theil
  Gini <- readSource('Gini')
  years <- getYears(Gini)
  TheilT <- TheilT.from.sigma(sigma.from.Gini(Gini))
  
  # population (in 1e6)
  pop <- calcOutput(type = 'Population', PopulationFuture="SSP_completed", aggregate = FALSE)
  sspnames <- paste0('SSP',1:5)
  pop <- pop[,years,paste0('pop_',sspnames)]
  getNames(pop) <- sspnames
  getSets(pop) <- c("iso3c","year","scenario")
  
  # GDP (in 1e6)
  GDPppp <- calcOutput(type = 'GDPppp', GDPpppFuture="SSP_completed", aggregate = FALSE)
  GDPnames <- getNames(GDPppp)
  GDPppp <- GDPppp[,years,paste0('gdp_',sspnames)]
  getNames(GDPppp) <- sspnames
  getSets(GDPppp) <- c("iso3c","year","scenario")
  
  # allocate empty objects for storing Theil contribution and weights
  contribTheilT <- pop
  contribTheilT[,,] <- NA
  s_i <- pop
  s_i[,,] <- NA
  
  # contribution to Theil index depends on region mapping. We always use the one specified in getConfig().
  regionmapping <- read.csv(toolMappingFile(type = 'regional', name = getConfig()$regionmapping), sep = ';', colClasses = 'character')
  # GDP per capita
  xbar_i <- GDPppp/pop
  for (rr in unique(regionmapping$RegionCode)){
    rrCountries <- regionmapping$CountryCode[regionmapping$RegionCode == rr]
    # regional GDP per capita
    GDPppp_rr <- dimSums(GDPppp[rrCountries,,], dim = 1)
    Ntot_rr <- dimSums(pop[rrCountries,,], dim = 1)
    xbar_rr <- GDPppp_rr/Ntot_rr
    # contribution to Theil index (unweighted)
    contribTheilT[rrCountries,,] <- TheilT[rrCountries,,] + log(xbar_i[rrCountries,,]/xbar_rr)
    # weights = income share of country i: 
    # s_i = N_i/N * xbar_i/xbar = GDP_i/GDP_rr
    s_i[rrCountries,,] <- GDPppp[rrCountries,,]/GDPppp_rr
    # sanity check: ensure that weights for a region sum to one (within floating point precision)
    assert_that(max(abs(dimSums(s_i[rrCountries,,],dim=1) - 1)) < 1e-10)
  }
  
  # for easier REMIND integration use same names as GDP scenarios for Theil
  # change this if we later want to test effect of per capita income growth vs. inequality
  getNames(contribTheilT) <- GDPnames
  getNames(s_i) <- GDPnames
  
  return(list(x = contribTheilT, weight = s_i, unit = "-", description = 'aggregated: Theil-T index, not-aggregated: unweighted contribution to Theil-T'))
}

##  helper functions. 
TheilT.from.sigma <- function(sigma){
  # Theil T coefficient for lognormal distribution
  TheilT <- sigma^2/2.
  return(TheilT)
}

sigma.from.Gini <- function(G){
  # assuming lognormal distribution: convert Gini to sigmas
  sigma <- sqrt(2)*qnorm((G+1)/2)
  return(sigma)
}



