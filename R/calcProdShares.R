#' @title Manufacture production shares for spc and wind
#' @description Shares of world manufacture for spv modules and wind turbines for 2018 and 2019
#' @author Aman Malik
#' @return magpie object with REMIND-aggregated region



calcProdShares <- function(){
  x <- readSource(type = "ProdShares")
  
return(list(x=x, 
            weight=NULL,
              unit="",
            description= "share of world manufacture for solar pv modules and wind turbines"))
  
}