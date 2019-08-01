calcTaxXport <- function() {
  x <- readSource("REMIND_11Regi", subtype="xpres_tax")
  
  weight <- new.magpie(getRegions(x),getYears(x),getNames(x),fill=1)   # or use export, from IEA?
  
  x <- time_interpolate(x,c(seq(2010,2150,5)),extrapolation_type="constant",integrate_interpolated_years=TRUE)
    
  return(list(x=x,weight=weight,
              unit="$/GJ", 
              description="resource export taxes, not used in default settings."
  ))
}