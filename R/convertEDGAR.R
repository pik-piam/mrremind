convertEDGAR <- function(x,subtype) {
  
  if(subtype=="ch4waste" | subtype=="n2owaste" | subtype=="co2" | subtype=="CO" | subtype=="NOx" | subtype=="VOC" | subtype=="NH3" | subtype=="SO2" | subtype=="PM10" | subtype =="GHG") {
    # split regional data
    # "ANT" -> "SXM", "CUW"
    # "SCG" -> "SRB", "MNE"
    m <- matrix(c(c("ANT","ANT","SCG","SCG"),c("SXM","CUW","SRB","MNE")),4)
    w <- calcOutput("Population",aggregate=FALSE)[c("SXM","CUW","SRB","MNE"),2010,"pop_SSP2"]
    x_split <- toolAggregate(x[c("ANT","SCG"),,],m,weight=w)
    # delete ANT and SCG from x
    x <- x[c("ANT","SCG"),,invert=TRUE]
    x <- mbind(x,x_split)
  } else if(subtype=="ch4_history") {
    # split regional data
    # "SCG" -> "SRB", "MNE"
    m <- matrix(c(c("SCG","SCG"),c("SRB","MNE")),2)
    w <- calcOutput("Population",aggregate=FALSE)[c("SRB","MNE"),2005,"pop_SSP2"]
    x_split <- toolAggregate(x["SCG",,],m,weight=w)
    # delete SCG from x
    x <- x[c("SCG"),,invert=TRUE]
    x <- mbind(x,x_split)
    x[is.na(x)] <- 0
    x <- toolISOhistorical(x)
  } else if(subtype=="HFC") {
    # split regional data
    # "SCG" -> "SRB", "MNE"
    m <- matrix(c(c("SCG","SCG"),c("SRB","MNE")),2)
    w <- calcOutput("Population",aggregate=FALSE)[c("SRB","MNE"),2005,"pop_SSP2"]
    x_split <- toolAggregate(x["SCG",,],m,weight=w)
    # delete SCG from x
    x <- x[c("SCG"),,invert=TRUE]
    x <- mbind(x,x_split)  
  }  

  
  if(subtype=="ch4waste" | subtype=="n2owaste" | subtype=="co2" | subtype=="ch4_history" | subtype=="CO" | subtype=="NOx" | subtype=="VOC" | subtype=="NH3" | subtype=="SO2" | subtype=="PM10") {
    # In the EDGAR data source shipping and aviation emissions are reported in global values in extra regions called AIR and SEA
    # put international shipping (SEA) and international aviation emissionen (AIR) in a seperate dimension
    x_SEA_AIR <- new.magpie("GLO",getYears(x),c("SEA","AIR"))
    x_SEA_AIR[,,"SEA"] <- x["SEA",,"TOTAL"]
    x_SEA_AIR[,,"AIR"] <- x["AIR",,"TOTAL"]
    # delete SEA and AIR from regional dimension
    # and allocalte to all countries 
    x <- x[c("SEA","AIR"),,invert=TRUE]
    m <- matrix(c(getRegions(x),rep("GLO",length(getRegions(x)))),length(getRegions(x)))
    w <- new.magpie(getRegions(x),getYears(x),fill=1)      # FIXME use GDP as weight
    x_add <- toolAggregate(x_SEA_AIR,m,weight=w)
    # Allocate to existing but empty variables in x
    x[,,"1C1"] <- x_add[,,"AIR"]
    x[,,"1C2"] <- x_add[,,"SEA"]
  #} else if(subtype=="CO" | subtype=="NOx" | subtype=="VOC" | subtype=="NH3" | subtype=="SO2" | subtype=="PM10") {
  #  # delete SEA and AIR from regional dimension
  #  x <- x[c("SEA","AIR"),,invert=TRUE]
  }
  
  # fill all missing countries with 0
  x <- toolCountryFill(x,fill=0)
  return(x)
}  