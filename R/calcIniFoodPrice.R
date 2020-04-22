#' @title calcIniFoodPrice
#'
#' @description provides global prices from the IMPACT model projections for MAgPIE commodities
#' for the initialization of the flexible demand model.
#'
#' @note The IMPACT projections start in 2005 and the prices are taken from that year. 
#'
#' @param datasource The datasource specification. Currently available \code{FAO} and \code{IMPACT3.2.2World_Price}.
#' @param year Specifies the year for the initialization of prices in MAgPIE. Default is \code{y2005}.
#' @param products subselection of products to be returned
#'
#' @return List with a magpie object with commodity prices on global level in $05/tDM.
#' @author Mishko Stevanovic, Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{readIMPACT3.2.2World_Price}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("IniFoodPrice", aggregate=FALSE)
#' }
#' 
#' @importFrom stringr str_sub
#' @importFrom magpiesets findset

calcIniFoodPrice <- function(datasource="FAO", year="y2005", products="kfo"){
  
  # dry matter factors
  dm <- 1/readSource("ProductAttributes","Products")[,,"wm"]
  dm <- collapseNames(dm)
  
  if(datasource=="IMPACT3.2.2World_Price"){
    out <- readSource("IMPACT3.2.2World_Price")
    # select scenario and initial year
    out <- collapseNames(out[,year,"SSP2-NoCC-NoCC-379"])
    out <- setYears(out, NULL)
    # get the mapping 
    mapping <- read.csv(toolMappingFile("sectoral","impact2magpie.csv"), na.strings="")
    mapping <- mapping[1:nlevels(mapping$MAgPIE),]
    out <- toolAggregate(out, rel=mapping, from="IMPACT", to="MAgPIE", dim=3.1, partrel=TRUE)
    
    out<-add_columns(x=out,dim = 3.1,addnm = setdiff(findset("kall"),getNames(out)))
    
    # manually add prices for sugr_cane, sugr_beet, alcohol, fish, oilpalm and fodder from exteral sources 
    
    out[,,"sugr_cane"] <- 13.07939764 # the FAO producer price in Brazil in 2005 (biggest production)
    out[,,"sugr_beet"] <- 55.311522   # the FAO producer price in France in 2005 (biggest production)
    out[,,"alcohol"]   <- 300         # a guess that a liter of beer is 1$ # BB: this is luxury. I would go for 30 cents like Sternie (also beacuse we are interested in the pirmary products going into alcohol, not the marketed product).
    out[,,"fish"]      <- 2067.626632 # OECD/FAO price of fish food traded in 2005
    out[,,"oilpalm"]   <- 100         # palmoilpoint.blogspot.de (527 malaysian ringgit = 118 USD rounded to 100)
    out[,,"oilcakes"]   <- 300        # https://www.indexmundi.com/commodities/?commodity=soybean-meal&months=180
    out[,,"cottn_pro"]   <- 700       # https://cotton.ces.ncsu.edu/wp-content/uploads/2018/02/Seed-Cotton-Summary_Feb-13-2018.pdf?fwd=no 0.3353*2.2*1000
    out[,,"foddr"]     <- 200         # equivalent to cereals
    out[,,"brans"]     <- 100         # quick google search
    out[,,"distillers_grain"] <- 200  # quick google search
    out[,,"ethanol"]   <- 900         # quick google search
    out[,,"fibres"]    <- 1500        # https://cotton.ces.ncsu.edu/wp-content/uploads/2018/02/Seed-Cotton-Summary_Feb-13-2018.pdf?fwd=no 0.690*2.2*1000
    out[,,"molasses"]  <- 50          # quick google search
    out[,,"wood"]      <- 150         # quick google search
    out[,,"woodfuel"]  <- 100         # quick google search
    out[,,"begr"]      <- 50          # quick google search
    out[,,"betr"]      <- 50          # quick google search
    out[,,"res_cereals"]      <- 50   # quick google search
    out[,,"res_fibrous"]      <- 50   # quick google search
    out[,,"res_nonfibrous"]   <- 50   # quick google search
    out[,,"pasture"]      <- 50       # quick google search
    out[,,"scp"]      <- 1500         # using upper end prices similar to fishmeal
    
    # correct the prices for dry matter values  
    out <- out[,,]/dm[,,]
    
    
    description=paste0("Prices from the ",datasource," model projections plus crude estimates for missing commodities for year ",year,".")
    
  } else if(datasource=="FAO"){
    out <- calcOutput(type="PriceAgriculture", datasource="FAO", aggregate="GLO")
    out <- out[,year,]
    out <- collapseNames(out)
    out <- add_columns(x=out,dim = 3.1,addnm = setdiff(findset("kall"),getNames(out)))
    
    
     out[,,"fish"]      <- 2067.626632 # OECD/FAO price of fish food traded in 2005
     out[,,"oilpalm"]   <- 100         # palmoilpoint.blogspot.de (527 malaysian ringgit = 118 USD rounded to 100)
     out[,,"oilcakes"]   <- 300        # https://www.indexmundi.com/commodities/?commodity=soybean-meal&months=180
     out[,,"brans"]     <- 100         # quick google search
     out[,,"distillers_grain"] <- 200  # quick google search
     out[,,"ethanol"]   <- 900         # quick google search
     out[,,"alcohol"]   <- 300         # a guess that a liter of beer is 1$ # BB: this is luxury. I would go for 30 cents like Sternie (also beacuse we are interested in the pirmary products going into alcohol, not the marketed product).
     out[,,"molasses"]  <- 50          # quick google search
     out[,,"wood"]      <- 150         # quick google search
     out[,,"woodfuel"]  <- 100         # quick google search
     out[,,"begr"]      <- 50          # quick google search
     out[,,"betr"]      <- 50          # quick google search
     out[,,"res_cereals"]      <- 50   # quick google search
     out[,,"res_fibrous"]      <- 50   # quick google search
     out[,,"res_nonfibrous"]   <- 50   # quick google search
     out[,,"pasture"]      <- 50       # quick google search
     out[,,"scp"]      <- 1500         # using upper end prices similar to fishmeal
     
    # foddr price seems unrealistic low

    # correct the prices from online sources for dry matter values  
     out <- out/dm

    
    # set years to NULL
    out <- setYears(out, NULL)
    
    description=paste0("Prices from the ",datasource," model projections plus crude estimates for missing commodities for year ",year,".")
  }
  
  out<-out[,,findset(products,noset = "original")]
  
  return(list(x=out,
              weight=NULL,
              unit="US$05/tDM",
              description=description,
              isocountries=FALSE))
}