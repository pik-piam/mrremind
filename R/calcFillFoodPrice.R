#' @title calcFillFoodPrice
#' @description 
#' Fills some (75% of all countries with some reporting, 50% of total data) food prices at the MAgPIE product level
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @param datasource FAO by default
#' @param indicator MER or PPP dollar indices
#' @author David Chen
#' @importFrom luscale superAggregate

calcFillFoodPrice <- function(datasource="FAO", indicator="MER"){

if (indicator=="MER"){
prices <- readSource("FAO", "PricesProducerAnnual")
}
  
else if (indicator=="LCU") {
  prices <- readSource("FAO", "PricesProducerAnnualLCU")
}
#production quantities for aggregation
CropPrim <- readSource("FAO", "Crop")[,,"production"]
livstPrim <- readSource("FAO", "LivePrim")[,,"production"]
yearsPrim <- intersect(getYears(CropPrim),getYears(livstPrim))
prod <- mbind(CropPrim[,yearsPrim,], livstPrim[,yearsPrim,])
prod <- collapseNames(prod)
years_p <- intersect(getYears(prod),getYears(prices))
prod <- prod[,years_p,]

prices <- prices[,years_p,intersect(getNames(prices),getNames(prod))]
prod <- prod[,,intersect(getNames(prices),getNames(prod))]

#aggregate to magpie products
aggregation <- toolGetMapping("FAOitems.csv", type = "sectoral", where="mappingfolder")
prices <- toolAggregate(prices, rel=aggregation, from="ProductionItem", 
                        to="k", dim=3.1, partrel = TRUE, verbosity=2, weight=prod)
#time interpolate between-year gaps - gaps longer than 1 year, does not improve
# pricest <- time_interpolate(prices, interpolated_year = c(1990:2013), 
#                            integrate_interpolated_years = TRUE)


#population weight
pop <- calcOutput("Population", aggregate=F)[,,"pop_SSP2"]
pop <- time_interpolate(pop, interpolated_year=getYears(prices))


maiz <- prices/dimSums(prices[,,"maiz"], dim=3)
maiz[is.infinite(maiz)] <- 0
maiz <- superAggregate(maiz, level="glo", aggr_type="weighted_mean", 
                      weight=magpie_expand(pop,prices))
prices_maiz <- maiz*dimSums(prices[,,"maiz"], dim=3)
prices_maiz[which(prices>0)] <-0
prices1 <- prices_maiz+prices

chick <- prices/dimSums(prices[,,"livst_chick"], dim=3)
chick[is.infinite(chick)] <- 0
chick <- superAggregate(chick, level="glo", aggr_type="weighted_mean", 
                       weight=magpie_expand(pop,prices))
prices_chick <- chick*dimSums(prices[,,"livst_chick"], dim=3)
prices_chick[which(prices1>0)] <-0
prices2 <- prices_chick+prices1

soy <- prices/dimSums(prices[,,"soybean"], dim=3)
soy[is.infinite(soy)] <- 0
soy <- superAggregate(soy, level="glo", aggr_type="weighted_mean", 
                        weight=magpie_expand(pop,prices))
prices_soy <- soy*dimSums(prices[,,"soybean"], dim=3)
prices_soy[which(prices2>0)] <-0
prices3 <- prices_soy+prices2


milk <- prices/dimSums(prices[,,"livst_milk"], dim=3)
milk[is.infinite(milk)] <- 0
milk <- superAggregate(milk, level="glo", aggr_type="weighted_mean", 
                      weight=magpie_expand(pop,prices))
prices_milk <- milk*dimSums(prices[,,"livst_milk"], dim=3)
prices_milk[which(prices3>0)] <-0
prices4 <- prices_milk+prices3

x <- prices4


  return(list(x=x,
              weight=NULL, 
              unit= "million $/ton", 
              description="magpie product prices"))
  
}
#> length(where(prices4>0)$true$individual)
#[1] 217683
#> length(where(prices4==0)$true$individual)
#[1] 63117


# Marginal returns not worth it at this point.. 1000 out of 20000 values filled 

# potato <- prices/dimSums(prices[,,"potato"], dim=3)
# potato[is.infinite(potato)] <- 0
# potato<- superAggregate(potato, level="glo", aggr_type="weighted_mean", 
#                       weight=magpie_expand(pop,prices))
# 
# prices_potato <- potato*dimSums(prices[,,"potato"], dim=3)
# prices_potato[which(prices4>0)] <-0
# 
# prices5 <- prices_potato+prices4




