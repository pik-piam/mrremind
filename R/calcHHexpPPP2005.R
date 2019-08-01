calcHHexpPPP2005 <- function(){

  constantPPP2011 <- readSource("WDI",subtype = "NE.CON.PRVT.PP.KD")
  currentPPP <- readSource("WDI",subtype = "NE.CON.PRVT.PP.CD")
  
  index = constantPPP2011 / setYears(constantPPP2011[,2005,])
  constantPPP2005 = setYears(currentPPP[,2005,]) * index
  
  constantPPP2005[which(constantPPP2005 == "Inf")] = 0
  constantPPP2005[which(constantPPP2005 == "NaN")] = 0
  
  return(list(x= constantPPP2005,weight=NULL,
              unit = "PPP 2005$",
              description = "Household consumption expenditures"))
  
  }