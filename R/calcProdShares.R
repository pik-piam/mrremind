calcProdShares <- function(){
  x <- readSource(type = "ProdShares")
  
return(list(x=x, 
            weight=x,
              unit="",
            description= "share of world manufacture for solar pv modules and wind turbines"))
  
}