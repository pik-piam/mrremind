convertNIR <- function(x) {

  # add data for USA and CAN for 2005 by hand based on data in different data format from 2014
  x_add <- new.magpie(c("USA","CAN"),getYears(x),getNames(x),fill=0)
  x <- mbind(x,x_add)

  x["USA",2005,"Oil"] <- 1373.51
  x["USA",2005,"Gas"] <- 7240.27
  x["USA",2005,"VentingOil"] <- 0
  x["USA",2005,"VentingGas"] <- 0
  x["USA",2005,"FlaringOil"] <- 0
  x["USA",2005,"FlaringGas"] <- 0

  x["CAN",2005,"Oil"] <- 260.1
  x["CAN",2005,"Gas"] <- 909.04
  x["CAN",2005,"VentingOil"] <- 819.51
  x["CAN",2005,"VentingGas"] <- 227.59
  x["CAN",2005,"FlaringOil"] <-   2.51
  x["CAN",2005,"FlaringGas"] <-   0.33

  # fill all missing countries with 0
  x <- toolCountryFill(x, fill = 0, verbosity = 2)
  return(x)
}
