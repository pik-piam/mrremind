#' Calculates the share of distributed solar pv in total pv capacity. Only includes grid-connected pv.
#' @details Known limitations - source for distributed spv (IEA Renewables 2019), source for total spv (IRENA 2019)

calcDspvShare <- function()
{
  # Note: This method to calculate the share of distributed solar pv has limitations. Foremost is that
  # the sources for IEA (for distributed solar pv), and IRENA (for total spv)
  dspv <- readSource("IEA_REN")[,2018,]
  total_spv <- calcOutput("Capacity",subtype="capacityByTech",aggregate = F)[,2018,"spv"]*1000
  
  # to avoid getting NaN while dividing by total spv capacity, give countries with no spv, a very small value
  total_spv[which(total_spv==0),,] <- 0.0001 # in GW
  share <- new.magpie(getRegions(dspv),c(2020,2050))
  # assign 2020 shares same value as 2018
  share[,2020,] <- as.numeric(dspv/total_spv)
  
  # for a few countries with share greater than 1, force it to be 90%. This error might be due to the different
  # data sources but only for a small number of unimportant countries
  share[which(share>1),2020,] <- 0.9
  
  # share of dspv until 2050 converges to 0.5 linearly. 
  share[,2050,] <- 0.5
  share <- time_interpolate(share,seq(2025,2045,5),integrate_interpolated_years = T)
  
  
  return(list(x           = share,
              weight      = total_spv,
              unit        = "none",
              description = "share of distributed solar pv in total pv"))
}
