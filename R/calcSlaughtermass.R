#' @importFrom magpiesets findset
calcSlaughtermass<-function(){
  kap <- findset("kap")
  mb<-calcOutput("FAOmassbalance",aggregate = FALSE)
  production <- collapseNames(mb[,,kap][,,"dm"][,,"production"])
  slaughter_factor <- collapseNames(calcOutput("Attributes",subtype = "SlaughterFactor",aggregate = FALSE))
  attributes_living_animals<- calcOutput("Attributes",subtype = "LivingAnimals",aggregate = FALSE)
  
  slaughtermass=production*slaughter_factor*attributes_living_animals
  return(list(x=slaughtermass,
              weight=NULL,
              unit="Mt DM,Nr,P,K,WM or PJ gross energy",
              description="Mass of slaughtered animals.")
  )                   
}

