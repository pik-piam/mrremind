#' @importFrom magclass dimOrder

calcSlaughterFeedShare<-function(balanceflow=TRUE){
  
  fbask                  <- calcOutput("FeedBaskets", aggregate = FALSE)
  getNames(fbask, dim=1) <- paste0("alias_", getNames(fbask, dim=1))
  attributes             <- calcOutput("Attributes", aggregate = FALSE)
  fbask                  <- dimSums(fbask * attributes, dim=3.2)
  getNames(fbask, dim=1) <- substring(getNames(fbask, dim=1),7)
  k                      <- getNames(fbask, dim=1)
  
  
  if(balanceflow==TRUE){
    
    fbaskbalance <- calcOutput("FeedBalanceflow", aggregate = FALSE, per_livestock_unit=TRUE)
    fbaskbalance <- dimSums(fbaskbalance*attributes, dim=3.2)
  
    fbask        <- fbask + fbaskbalance[,,k]
    
    # set overcorrected negative values to zero
    fbask[which(fbask<0)] <- 0
  }
  
  slaughter_factor          <- collapseNames(calcOutput("Attributes", subtype = "SlaughterFactor", aggregate = F))[,,k]
  attributes_living_animals <- calcOutput("Attributes", subtype = "LivingAnimals", aggregate = F)[,,k]
  
  weight <- fbask
  SlaughterFeedShare <- slaughter_factor * attributes_living_animals / weight
  # limit to max 0.85
  SlaughterFeedShare[SlaughterFeedShare>0.85]=0.85
  
  #weight<-dimOrder(weight,c(1,3,2))
  out<-toolNAreplace(SlaughterFeedShare , weight)
  
  
  return(list(x=out$x,
              weight=out$weight,
              unit="Share of DM,Nr,P,K,WM or gross energy",
              description="Share of feed intake that gets withdrawn by slaughtermass per product",
              min=0,
              max=0.85)
  )                   
}


