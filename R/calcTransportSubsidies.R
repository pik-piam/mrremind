#' @title calc transport subsidies
#' @description provides transport subsidies
#'
#' @return magpie object of the transport subsidies data
#' @author Renato Rodrigues
#' @examples
#'
#' \dontrun{
#' calcOutput("TransportSubsidies")
#' }
#'

calcTransportSubsidies <- function(){

  transpSub <- readSource(type="TransportSubsidies")

  # mapping subsidies to technologies in remind
  map <- c("BEV"="apCarElT", "FCEV"="apCarH2T")

  # setting subsidy to average between private and legal entities subsidies
  out <- setNames(dimSums(transpSub[,,names(map)],dim=3.1)/2,as.vector(map))

  # convert subsidies from euros to trillions of dollares
  # inflation dollars: 1 $2005 eq to 1.3504 $2020
  # exchange rate 2020: 1 euro = 1.12 dollar
  # conversion from US$2005 to EUR2020: inflation/exchange rate = 1.3504/1.12 = 1.205714286
  out <- (out/1.205714286)/10^12

  # convert from cars to total (650 million cars in the World)
  out <- out*650*10^6

  #weight: using 2020 light duty vehicles final energy as weight
  fe <- calcOutput("FEdemand", aggregate = FALSE)[,2020,"gdp_SSP2.ueLDVt"]

  return(list(x=out, weight=fe,
               unit="trillion U.S. dollar",
               description="Total transport subsidies for BEV (apCarElT) and FCEV (apCarH2T)"
  ))

}
