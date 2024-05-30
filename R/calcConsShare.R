#This file is to generate income shares for 12 remind regions. The calculation 
#design would be to firstly calculate sigma from Gini, and assume a lognormal distribution,
#with sigma, integrate for 10 deciles 3) normalize to get decile share.
#But we will need the income share for 12 REMIND regions. The aggregation can 
#happen differently, 1)after the country level income shares is computed, adding 
#new pop and gdp data and compute the regional income share. 2) Before the income
#share calculation, on the inequality index (Gini/Theil)
#I'd like to go with 2) as regionla Theil has already be computed by Bjoern, and 
#also the approach with aggregating share encounters the problem of negleting wi-
#in-group inequality

calcConsShare <- function(){
  
  #get Theil index for 12 remind regions
  TheilT <- calcOutput(type = "Theil", aggregate=TRUE)
  
  
  #-------1.Calculate sigma from Theil-----------
  sigma <- sqrt(2*TheilT)

  
  #------2.From sigma to income shares-----------
  #Helper function compute 10 income group share given sigma
  DecShare.from.sigma<-function(sigma) {
    #Assume a mu, which doesn't affect the result
    mu <- 1
    
    # Find decile boundaries
    deciles <- qlnorm(seq(0, 1, by=0.1), meanlog = mu, sdlog = sigma)
    
    # Function for the integrand x * f(x)
    integrand <- function(x) {
      x * dlnorm(x, meanlog = mu, sdlog = sigma)
    }
    
    # Compute income shares by integrating over each decile range
    income_shares <- numeric(length(deciles) - 1)
    
    for (i in 2:length(deciles)) {
      income_shares[i - 1] <- integrate(integrand, lower = deciles[i - 1], upper = deciles[i])$value
    }
    
    # Normalize shares to sum to 100%
    total_income <- sum(income_shares)
    normalized_shares <- (income_shares / total_income) 
    
    return(normalized_shares)
    
  }
  
  #Create empty objects for storing income share
  ConsShare <- array(NA, dim =  c(dim(sigma),10),
                     dimnames = c(dimnames(sigma), list("decile" = 1:10)))
  
  
  # Applying function across each element in the magclass object
  for (i in 1:dim(sigma)[1]) {
    for (j in 1:dim(sigma)[2]) {
      for (k in 1:dim(sigma)[3]) {
        # Safely apply the DecShare.from.sigma function
        result <- tryCatch({
          DecShare.from.sigma(sigma[i, j, k])
        }, error = function(e) rep(NA, 10))  # Handle possible errors by returning NAs
        
        # Store the result in the appropriate slice of the expanded_data
        ConsShare[i, j, k, ] <- result
      }
    }
  }
  

  return(list(x = ConsShare,
              weight = NULL, 
              unit = "Dimentionless",
              description = "Consumption share of 10 sub-regional income deciles for Remind regions"))
}


