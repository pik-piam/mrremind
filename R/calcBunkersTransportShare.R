#' @importFrom quitte as.quitte
#' @importFrom stats lm predict

calcBunkersTransportShare <- function() {
  
  # read in cdiac data and convert into MtCO2
  cdiac <- readSource("CDIAC")[,,"Bunker"] * 44/12/1000
  
  # select historical time to be used
  last <- getYears(cdiac)[length(getYears(cdiac))]
  last <- as.numeric(gsub("y","",last))
  t    <- seq(1989,last,1)
  # generate whole needed time period
  t_all <- seq(2005,2150,5)
  
  # make empty magpie-object
  fit <- new.magpie(getRegions(cdiac),t_all)
  
  # make linear extrapolation
  for( r in getRegions(cdiac)) {
     # transfer into a quitte object
     x <- as.quitte(cdiac[r,t,])
     y <- x$value
     fit_lin <- lm( y~t )
     # allocate fit to the right region in the magpie-object
     fit[r,,] <- predict(fit_lin, data.frame(t=t_all))
     # if you like to make a plot
     #plot(t,y,xlim=c(1990,2050),title(r))
     #lines(t_all,predict(fit_lin, data.frame(t=t_all)), col='green')
  }
  # use the historical data from CDIAC fro 2005 and 2010
  fit[,c(2005,2010),] <- cdiac[,c(2005,2010),]
  
  # convert fitted emissions into energy:
  # use exogeneous, fixed demand emission factor as used in reporting, taken from www.eia.gov/oiaf/1605/excel/Fuel%20EFs_2.xls
  fit_energy <- fit[,t_all,]/69.3
    
  # use EDGE fedie data for SSP2
  fedie <- calcOutput("FEdemand",subtype = "FE",aggregate=FALSE)[,t_all,"gdp_SSP2.ueHDVt"]
  
  # calculate share
  share <- setNames(fit_energy/fedie,NULL)
  # substitute NA and Inf by 0
  share[is.na(share)]       <- 0
  share[is.infinite(share)] <- 0
  
  ### manipulate the calculated shares
  # make sure that share does not decrease strong (AFR, IND)
  for (step in c(2:length(getYears(share)))){
    for (reg in getRegions(share)){
      share[reg,getYears(share)[step],] <- 
        max(share[reg,getYears(share)[step],],share[reg,getYears(share)[step-1],])
    }
  }
  # calculate convergence shares
  conv_share <- share
  for (step in c(2:length(getYears(conv_share)))){
    for (reg in getRegions(conv_share)){
      conv_share[reg,getYears(conv_share)[step],] <- 
        0.4 * 
        (as.numeric(substring(getYears(conv_share)[step],2,5))-as.numeric(substring(getYears(conv_share)[2],2,5)))/(2150-as.numeric(substring(getYears(conv_share)[2],2,5))) + 
        conv_share[reg,getYears(conv_share)[step],] * 
        (2150 - as.numeric(substring(getYears(conv_share)[step],2,5)))/(2150-as.numeric(substring(getYears(conv_share)[2],2,5)))
    }
  } 
  
  return(list(x           = conv_share,
              weight      = fedie, 
              unit        = "trillion 2005US$", 
              description = "share of bunkers in nonldv liquid transport fe (ueHDVt)"))
}
