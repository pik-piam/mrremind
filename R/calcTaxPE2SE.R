calcTaxPE2SE <- function() {
  data <- readSource("REMIND_11Regi", subtype="pe2se")
  getYears(data) <- "y2005"
  w    <- new.magpie(getRegions(data),getYears(data),getNames(data),fill=1)
    
  return(list(x=data,weight=w,
              unit="USD(2005) per GJ",
              description="Taxes/subsidies are given in USD(2005) per GJ"))
}