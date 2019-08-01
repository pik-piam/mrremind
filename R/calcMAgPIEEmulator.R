calcMAgPIEEmulator <- function() {
  x <- readSource("REMIND_11Regi","biomass")
  # use same weight for all countries -> arithmetic mean
  w <- new.magpie(getRegions(x),2005,getNames(x),fill=1)
  return(list(x=x, weight=w,
              description = paste0("Parameters for biomass price 
                              supply curve derived from MAgPIE")))
}
