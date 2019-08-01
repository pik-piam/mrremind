#' Calculate hydro potential
#' 
#' Provides wind onshore potential data
#' 
#' 
#' @return wind onshore potential data and corresonding weights as a list of
#' two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readNREL}},
#' \code{\link{convertNREL}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("PotentialWind")
#' 
#' }
#' @importFrom magclass getNames
calcPotentialWind <- function() {
  
  # read wind data 
  nrel <- readSource("NREL",subtype="onshore")

  # we use only "near" in REMIND
  techPot <- collapseNames(nrel[,,"near"])
  
  # delete total
  techPot <- techPot[,,"total",invert=TRUE]
  # allocate c1-c9 to the right grades
  getNames(techPot) <- gsub("c1","9",getNames(techPot))                 
  getNames(techPot) <-gsub("c2","8",getNames(techPot))
  getNames(techPot) <-gsub("c3","7",getNames(techPot))
  getNames(techPot) <-gsub("c4","6",getNames(techPot))
  getNames(techPot) <-gsub("c5","5",getNames(techPot))
  getNames(techPot) <-gsub("c6","4",getNames(techPot))
  getNames(techPot) <-gsub("c7","3",getNames(techPot))
  getNames(techPot) <-gsub("c8","2",getNames(techPot))
  getNames(techPot) <-gsub("c9","1",getNames(techPot))
  
  # convert into EJ/a
  maxprod <- techPot * 1000 * 0.0036

  # add "nur" data
  nur <- new.magpie(getRegions(maxprod),getYears(maxprod),getNames(maxprod))
  nur[,,"9"] <- 0.09
  nur[,,"8"] <- 0.20
  nur[,,"7"] <- 0.24
  nur[,,"6"] <- 0.28
  nur[,,"5"] <- 0.32
  nur[,,"4"] <- 0.36
  nur[,,"3"] <- 0.40
  nur[,,"2"] <- 0.44
  nur[,,"1"] <- 0.48
  
  # put maxprod and nur together
  maxprod <- add_dimension(maxprod,dim=3.1,add="char",nm="maxprod")
  nur     <- add_dimension(nur,dim=3.1,add="char",nm="nur")
  data <- mbind(maxprod,nur)
  
  # dreate weight-matrix
  w <- new.magpie(getRegions(data),getYears(data),getNames(data),fill=1)
  w[,,"maxprod"] <- NA
    
  return(list(x=data,
              weight=w,
              unit="EJ/a",
              description="wind potential",
              mixed_aggregation=TRUE
               ))
}
