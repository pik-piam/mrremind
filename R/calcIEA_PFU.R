#' calcIEA_PFU
#' 
#' Computes data for the Primary Final Useful (PFU) resolution
#'
#' 
#' 
#' @return IEA data as MAgPIE object aggregated to country level
#' @author Antoine Levesque
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput("IEA_PFU")
#' }
#' 
#' @importFrom dplyr %>% 
#' @importFrom tidyr unite_

calcIEA_PFU <- function() {
  
  
  mapping <- toolGetMapping(type = "sectoral", name = "structuremappingFE_PFU.csv", returnPathOnly = TRUE)
  target = c("pfu")
  
  # read in data and convert from ktoe to EJ
  data <- readSource("IEA",subtype="EnergyBalances") * 0.0000418680000
  
  ### calculate data
  ieamatch <- read.csv2(mapping, stringsAsFactors = FALSE, na.strings ="" )
  regions  <- getRegions(data)
  years    <- getYears(data)
  #delete NAs rows
  ieamatch = ieamatch[c("iea_product","iea_flows",target)] %>% na.omit()
  #
  ieamatch = ieamatch %>% unite_("target",target, sep = ".")
  magpnames = ieamatch[["target"]]
  magpnames <- unique(magpnames)
  
  
  iea_pfu =  do.call(mbind,
                     lapply(magpnames, function(item) {
                       testdf = ieamatch[ieamatch$target == item ,c("iea_product","iea_flows")]
                       prfl <- paste(testdf[,"iea_product"],testdf[,"iea_flows"],sep=".")
                       prfl <- intersect(prfl, getNames(data))
                       tmp <- data[,,prfl] 
                       tmp <- dimSums(tmp,dim=3,na.rm = TRUE)
                       getNames(tmp) <- item
                       return(tmp)
                       
                     })
  )
  
  
  return(list(x=iea_pfu,weight=NULL,unit="EJ",
              description="IEA Data based on 2017 edition of IEA World Energy Balances with the PFU resolution"))
}
