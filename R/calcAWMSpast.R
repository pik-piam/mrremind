#' @importFrom magclass collapseNames setYears


calcAWMSpast<-function(){

  excretion<-collapseNames(calcOutput("Excretion",aggregate = F)[,,"confinement"][,,"nr"])
  awms_shr<-calcOutput("AWMSconfShrPast",aggregate = F)
  ef<-setYears(calcOutput("EF3confinement",selection=NULL,aggregate = F),NULL)
  awms<-excretion*awms_shr*ef
  
  return(list(x=awms,
              weight=NULL,
              unit="Mt Nr",
              description="Manure managed in various systems and its losses")
  )                   
}

