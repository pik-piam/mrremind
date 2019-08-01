

calcTCguess <- function() {
  
  y1 <- 2005
  y2 <- 1985
  
  tau    <- readSource("Tau","historical")  
  x      <- collapseNames(tau[,,"tau.total"])
  weight <- setYears(collapseNames(tau[,y1,"xref.total"]),NULL)
  
  tmp <- setYears(x[,y1,],NULL)/setYears(x[,y2,],NULL)
  tmp[tmp<1] <- 1
  tcguess <- tmp^(1/(y1-y2))-1
  
  return(list(x=tcguess,
              weight=weight,
              unit="-",
              description="Guess of annual TC in agriculture based on historic trends",
              note=c('data based on Dietrich J.P., Schmitz C., M\uFCller C., Fader M., Lotze-Campen H., Popp A.,',
                     'Measuring agricultural land-use intensity - A global analysis using a model-assisted approach',
                     'Ecological Modelling, Volume 232, 10 May 2012, Pages 109-118, ISSN 0304-3800, 10.1016/j.ecolmodel.2012.03.002.',
                     'preprint available \u40 http://edoc.gfz-potsdam.de/pik/display.epl?mode=doc&id=5281')))
}