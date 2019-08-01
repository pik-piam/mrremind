#' @title calcPriceAgriculture
#'
#' @description provides global prices from the IMPACT model projections, World Bank observations, and FAO 
#' obersvations for MAgPIE commodities in $/tDM
#' 
#' @param datasource Options of the source of data:  \code{IMPACT3.2.2World_Price}
#' , \code{FAO}, \code{FAOp} and \code{WBGEM}. 
#'
#' @return List with a magpie object with commodity prices on global level.
#' @author Mishko Stevanovic, Xiaoxi Wang
#' @seealso
#' \code{\link{readIMPACT3.2.2World_Price}},
#' \code{\link{calcWBGEM}},
#' \code{\link{readWBGEM}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("PriceAgriculture", datasource="IMPACT3.2.2World_Price", aggregate=FALSE)
#' calcOutput("PriceAgriculture", datasource="FAO")
#' calcOutput("PriceAgriculture",datasource = "WBGEM",aggregate =  FALSE)
#' }
#' 
#' @importFrom magpiesets findset reporthelper
#' @importFrom magclass wrap
#' @importFrom plyr ddply
#' @importFrom reshape2 melt acast
#' 

calcPriceAgriculture <- function(datasource="IMPACT3.2.2World_Price"){
  
  if(datasource=="IMPACT3.2.2World_Price"){
    out <- readSource(type="IMPACT3.2.2World_Price")
    # get the mapping 
    mapping <- toolGetMapping("impact2magpie.csv", type="sectoral")
    mapping <- mapping[-which(mapping$MAgPIE==""),]
    out <- toolAggregate(out, rel=mapping, from="IMPACT", to="MAgPIE", 
                           dim=3.2, partrel=TRUE, verbosity=2)
    # correct the prices for dry matter values  
    dm <- 1/readSource("ProductAttributes","Products")[,,"wm"]
    dm <- collapseNames(dm)
    commodities <- unique(mapping$MAgPIE)
    k_trade <- findset("k_trade")
    out <- out[,,]/dm[,,commodities]
    
    out <- add_dimension(out, dim=3.2, add="model", nm="IMPACT")
    names(dimnames(out))[3] <- "scenario.model.variable"
    
    description <- paste0("Prices from the IMPACT model projections. 
                          There are ",length(k_trade)-length(commodities),
                          " missing MAgPIE commodities: ", 
                          paste(k_trade[!k_trade %in% commodities], collapse=" "))
    weight <- NULL
    isocountries <- FALSE
    
  } 
  else if (datasource == "WBGEM") {
    x <- calcOutput("WBGEM",aggregate = FALSE)

    #sectoral mappings
    mapping <- toolMappingFile("sectoral","mappingWBGEM2MAgPIEnew.csv",readcsv = TRUE)
    mapping <- mapping[mapping$wbgem %in% getNames(x),]
    
    x <- x[,,mapping$wbgem]
    tmp <- ddply(mapping,"magpie",nrow)
    tmp <- base::merge(mapping,tmp,by="magpie")
    weight <- as.magpie(acast(melt(tmp),.~wbgem))
    getNames(weight) <- gsub("\\..","",getNames(weight))
    
    x<- toolAggregate(x,mapping,from = "wbgem",to = "magpie",weight =weight,dim=3)
    
    dm <- 1/readSource("ProductAttributes","Products")[,,"wm"]
    dm <- collapseNames(dm)
    k_trade <- findset("k_trade")
    
    out <- NULL
    for (i in getNames(x,dim=1)){
      out <- mbind(out,x[,,i]/setNames(dm[,,i],NULL)  )
      }
    rm(x)
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    names(dimnames(out))[3] <- gsub("data1","variable",names(dimnames(out))[3])
    
    description <- paste0("Average prices from the WBGEM-Commodities. 
                          There are ",length(k_trade)-length(getNames(out)),
                          " missing MAgPIE commodities: ", 
                          paste(k_trade[!k_trade %in% getNames(out)], collapse=" "))
    weight <- NULL
    isocountries <- FALSE
    
  } 
  else if(datasource=="FAOp"){
    # calculate prices as a ratio of production value and production quantities
    vprod <- calcOutput("AgProductionValue", aggregate=FALSE, datasource="FAO")
    qprod <- calcOutput("FAOharmonized", aggregate=FALSE)
    aggregation <- toolGetMapping("FAOitems.rda","sectoral",where="moinput")
    # aggregate FAO quantities into MAgPIE commodities 
    qprod <- toolAggregate(qprod[,,"production"], rel=aggregation, from="FoodBalanceItem", 
                             to="k", dim=3.1, partrel = TRUE, verbosity=2)
    qprod <- collapseNames(qprod)
    ## convert to DM tonnes
    dm <- 1/readSource("ProductAttributes","Products")[,,"wm"]
    dm <- collapseNames(dm)
    comms <- intersect(intersect(getNames(dm),getNames(qprod)), getNames(vprod,dim=3))
    qprod <- qprod[,,comms]*dm[,,comms]
    
    years <- intersect(getYears(qprod),getYears(vprod))
    out <- vprod[,years,comms]/qprod[,years,comms]
    weight <- qprod
    weight[is.na(out) | is.infinite(out)] <- 0
    out[is.na(out) | is.infinite(out)]    <- 0
    
    description <- "FAO prices Agricultural Value statistics."
    isocountries <- TRUE
  } 
  else if(datasource=="FAO"){
    out <- readSource("FAO", "PricesProducerAnnual", convert=TRUE)
    aggregation <- toolGetMapping("FAOitems.rda","sectoral",where="moinput")
    
    qprod <- collapseNames(calcOutput("FAOharmonized", aggregate=FALSE)[,,"production"])
    qprod <- qprod[,,-grep("Total", getNames(qprod))]
    qprod <- toolAggregate(qprod, rel=aggregation, from="FoodBalanceItem",
                           to="ProductionItem", dim=3, partrel=TRUE, verbosity=2)
    comms <- intersect(getNames(out), getNames(qprod))
    years <- intersect(getYears(out), getYears(qprod))
    qprod <- qprod[,years,comms]
    out   <- out[,years,comms]
    # weighted aggregation of fao prices to magpie commodities
    out <- toolAggregate(out, rel=aggregation, weight=qprod, from="ProductionItem", 
                       to="k", dim=3, partrel=TRUE, verbosity=2)
    out <- out[,,-which(getNames(out) %in% c("remaining","not_clear"),arr.ind=TRUE)]
    
    dm <- 1/readSource("ProductAttributes","Products")[,,"wm"]
    dm <- collapseNames(dm)
    out <- out/dm[,,getNames(out)]
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    names(dimnames(out))[3] <- gsub("data1","variable",names(dimnames(out))[3])
    
    weight <- toolAggregate(qprod, rel=aggregation, from="ProductionItem", 
                            to="k", dim=3.1, partrel = TRUE, verbosity=2)
    weight <- weight[,,-which(getNames(weight) %in% c("remaining","not_clear"),arr.ind=TRUE)]
    description <- "FAO prices based on Annual Produces Prices statistics."
    isocountries <- TRUE
  }
  
  return(list(x=out,
              weight=weight,
              unit="US$05/tDM",
              description=description,
              isocountries=isocountries))
}