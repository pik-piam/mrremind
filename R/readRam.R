#' Regional multiliers from Ram et al. , 2018

readRam <- function(){
  regional_multiplier <- read_excel("regional_multiplier.xlsx")
  x <- as.magpie(regional_multiplier,spatial=1)
  
  return (x)
}
  
