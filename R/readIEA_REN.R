#' Reads the distributed solar pv capacity from IEA Renewables report (2019). 
#' @details Capacity in GW. Distributed solar, defined in the IEA Renewables (2019), includes 
#' rooftop residential (0-10 kW, grid-connected), tooftop and ground-mounted commercial
#'  and industrial (10-1000kW, grid-connected), and off-grid (8W - 100 kW)
#' @author Aman Malik
#' @importFrom readxl read_excel


readIEA_REN <- function()
{
  input <- read_excel("IEA-dis-spv.xlsx")
  colnames(input)[1] <- "region"
  input <- as.magpie(input,spatial=1)
  getNames(input) <- "capacity"
  getSets(input) <- c("region","year","value")
  x <- input
  return(x)
}