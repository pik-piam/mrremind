#' calcILO
#' ILO data for employment in oil and gas; coal and lignite
#' @param subtype "Reliable" and "all". ILO classifies its data per country into reliable, unreliable, and break in series (years without data). "Reliable" contains both reliable and break-in-series-data, "all" contains both reliable and unreliable data
#' @return magpie object with employment data for different years

calcILO <- function(x,subtype){
  x <- readSource("ILO")
  x[is.na(x)] <- 0
  
  if(subtype=="Reliable")
  {
    x <- x[,,"Unreliable",pmatch=T,invert=T]
    x[,,"Oil and Gas. "] <- x[,,"Oil and Gas. "]+x[,,"Oil and Gas.Break in series"]
    x[,,"Coal and Lignite. "] <- x[,,"Coal and Lignite. "]+x[,,"Coal and Lignite.Break in series"]
    x <- x[,,c("Oil and Gas. ","Coal and Lignite. ")]
    x <- collapseNames(x)
    x <- magpiesort(x)
    
  }
  
  if(subtype=="all"){ # containts both unreliable and "break in series" labels
    x[,,"Oil and Gas. "] <- dimSums(x[,,"Oil and Gas",pmatch=T])
    x[,,"Coal and Lignite. "] <- dimSums(x[,,"Coal and Lignite",pmatch=T])
    x <- x[,,c("Oil and Gas. ","Coal and Lignite. ")]
    x <- collapseNames(x)
    x <- magpiesort(x)
  }
  
  return(list(
    x=x,
    weight=NULL,
    unit="000s",
    description="Employment in coal and lignite or oil and gas"))
}
