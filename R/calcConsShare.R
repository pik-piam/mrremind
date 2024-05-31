#This function computes consumption shares of sub-regional deciles in total consumption
#for 12 remind regions. The method is to assume a log-normal distribution for 
#the sub-national consumption distribution and integrate to get the share of 10 deciles groups.
#The calculation is done in 3 steps 1. calculate sigma (variance of the distribtution)
#from known inequality index (Gini/Theil). 2. generate the distribution with sigma,
#integrate to get relative share of each decile group. 3. normalize the consumption share. 
#
#But the inequality data is on country-level, we will need to aggregate to get shares 
#for 12 REMIND regions. The aggregation can happen at different steps: 
#(1) compute country level share firstly, and aggregate to regional shares with extra
#gdp and pop data.
#(2) aggregate regional level inequality index firstly, and then calculate regional income
#share.
#
#I'd like to go with 2) option, as regional Theil index has already been computed 
#by calcTheil function. And also the approach with aggregating share encounters the 
#problem of negleting within-group inequality.

calcConsShare <- function(){
  
  #get Theil index for 12 remind regions
  TheilT <- calcOutput(type = "Theil", aggregate=TRUE)
  
  
  #-------1.Calculate sigma from Theil-----------
  sigma <- sqrt(2 * TheilT)

  
  #------2.From sigma to income shares-----------
  #Helper function compute 10 income group share given sigma
  DecShare.from.sigma<-function(sigma) {
    #Assume any value of mu, which doesn't affect the result
    mu <- 1
    
    # Find decile boundaries
    deciles <- qlnorm(seq(0, 1, by=0.1), meanlog = mu, sdlog = sigma)
    
    # Function for the integrand x * f(x)
    integrand <- function(x) {
      x * dlnorm(x, meanlog = mu, sdlog = sigma)
    }
    
    # Compute shares by integrating over each decile range
    income_shares <- numeric(length(deciles) - 1)
    
    for (i in 2:length(deciles)) {
      income_shares[i - 1] <- integrate(integrand, lower = deciles[i - 1], upper = deciles[i])$value
    }
    
    # Normalize
    total_income <- sum(income_shares)
    normalized_shares <- (income_shares / total_income) 
    
    return(normalized_shares)
  }
  
  #Empty objects for storing income share
  ConsShare <- array(NA, dim =  c(dim(sigma),10),
                     dimnames = c(dimnames(sigma), list("decile" = 1:10)))
  
  
  # Applying function across each element in the magclass object
  for (i in 1:dim(sigma)[1]) {
    for (j in 1:dim(sigma)[2]) {
      for (k in 1:dim(sigma)[3]) {
        result <- tryCatch({
          DecShare.from.sigma(sigma[i, j, k])
        }, error = function(e) rep(NA, 10))  
        ConsShare[i, j, k, ] <- result
      }
    }
  }
  

  return(list(x = ConsShare,
              weight = NULL, 
              unit = "Dimentionless",
              description = "Consumption share of 10 sub-regional income deciles for Remind regions"))
}


