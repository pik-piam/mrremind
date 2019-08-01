#' @title calcAgProductionValue
#' 
#' @description  Calculate FAO Value Of Production
#' 
#' @param datasource Currently available: \code{"FAO"}
#'
#' @return FAO Value Of Production as a list of MAgPIE objects
#' 
#' @author Roman Popov, Mishko Stevanovic
#' @seealso \code{\link{calcOutput}}, \code{\link{readFAO}},
#' \code{\link{convertFAO}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- calcOutput("AgProductionValue", datasource="FAO")
#' }
#'  
calcAgProductionValue <- function(datasource="FAO") {

  if(datasource=="FAO"){
    data <- readSource("FAO", "ValueOfProd")
    data <- data[,,"Gross_Production_Value_(constant_2004_2006_million_US$)_(USD)"]
    data <- collapseNames(data)
    
    aggregation <- toolGetMapping("FAOitems.rda", type = "sectoral", where="moinput")
    
    data[is.na(data)] <- 0
    
    # remove data that contains the aggregate categories
    data <- data[,,-grep("(Total)", getNames(data), fixed=TRUE)]
    # remove live weight data
    data <- data[,,-grep("live weight", getNames(data), fixed=TRUE)]
    
    out <- toolAggregate(data, rel=aggregation, from="ProductionItem", to="k", 
                         dim=3.1, partrel = TRUE, verbosity=2)
    
    out <- collapseNames(out)  
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    description <- "FAO Value Of Production information aggregated to magpie categories"
  }
  
  names(dimnames(out))[3] <- "scenario.model.variable"
  
  return(list(x=out,
              weight=NULL, 
              unit= "million_US$05/yr", 
              description=description))
}
