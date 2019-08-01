#' Prices of CES variables for calibration runs in REMIND
#' 
#' Uses former REMIND runs
#' 
#' @author Antoine Levesque
calcCalibrationPrices <- function() {
  #----- Functions ------------------
  
  #----- READ-IN DATA ------------------
  prices = readSource("RemindCesPrices")
  
  #----- Parameters ------------------
  epsilon = 1e-4
  #----- Process data ------------------
  #______________________________________
  #|In case of new variables not present |
  #|in former runs, you can add them here|
  #|by using the available prices        |
  #|                                     |
  #|                                     |
  #|                                     |
  #|_____________________________________|
  
  #Weighting could be improved by using calcFEdemand, but some variables are missing
  # and prices are only instrumental
  weights = prices
  weights[,,] <- 1
 
  prices[prices < epsilon] = epsilon
  
  return(list(x=prices,weight=weights,
              unit = "CES variables derivatives in terms of inco",
              description = "CES variables derivatives in terms of inco"))
}
