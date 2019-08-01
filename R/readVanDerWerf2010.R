#' readVanDerWerf2010
#' 
#' Reads a dataset containing values for global fire emissions. Source: van der
#' Werf G. R., Randerson J. T., Giglio L., Collatz G. J., Mu M., Kasibhatla P.
#' S., Morton D. C., DeFries R. S., Jin Y., van Leeuwen T. T.: Global fire
#' emissions and the contribution of deforestation, savanna, forest,
#' agricultural, and peat fires (1997-2009)
#' 
#' 
#' @return A MAgPIE object containing emissionfactors per burned dry matter for
#' different causes.
#' @author Stephen Wirth
#' @examples
#' 
#' 
#'   \dontrun{
#'     x <- readSource("VanDerWerf2010")
#'   }
#' 
#' @importFrom reshape2 melt
readVanDerWerf2010 <- function(){
  file <- "ef.csv"
  #read file
  data <- read.csv(file = file, header = T,stringsAsFactors = F, skip=5)
  molten <- melt(data, id.vars = "dummy")
  #create vector for variable names
  rows <- molten$dummy
  cols <- molten$variable
  n <- paste(rows, cols, sep = ".")
  #get values
  value <- molten$value
  d <- new.magpie(years = "y2010", names = n,  sets = c("region", "years", "data") )
  d[,,] <- value
  
  return(d)
}
