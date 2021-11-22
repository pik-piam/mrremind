#' Calculates a multiplier for the employment factor for distributed solar pv
#' @author Aman Malik

calcDspvEFmult <- function()
  {
  
  share_notagg <- calcOutput("DspvShare",aggregate = F)
  
  share_agg <- calcOutput("DspvShare")
  
  ef_ceew <- a <- readSource("CEEW",convert=F,subtype="Employment factors")
  ef_ceew <- ef_ceew[,,c("Solar|PV.CI","Solar|PV|Rooftop.CI")]
  #multiplier for countries with low rooftop penetration
  mult_low <- as.numeric(ef_ceew[,,"Solar|PV|Rooftop.CI"]/ef_ceew[,,"Solar|PV.CI"])
  # multiplier for countries with high rooftop penetration
  mult_high <- 1.3 # avg. jobs in construction phase from IEA sustainable futures report Fig 2.3 
  # 10 for utility and 14 for rooftop. Assuming these values are weighted for capacity, the number represents 
  # mostly rooftop addition in China, Europe, and Japan
  
  mult_factor <- new.magpie(getRegions(share_notagg),getYears(share_notagg))
  slope <- (mult_high-mult_low)/(as.numeric(share_agg["EUR",2020,]-share_agg["IND",2020,]))
  intercept <- as.numeric(mult_high - (slope*share_agg["EUR",2020,]))
 # intercept <- as.numeric(mult_low - (slope*share_agg["IND",2020,]))
 
  for (i in getRegions(share_notagg)){
  if (share_notagg[i,2020,]>0.66)
    mult_factor[i,,] <- 1.3
  else {
    mult_factor[i,,] <- slope*share_notagg[i,,] + intercept
  }  
  }

  
  return(list(x           = mult_factor,
              weight      = share_notagg,
              unit        = "none",
              description = "multipler for Solar PV construction and installation employment factor"))
  
  }
  
