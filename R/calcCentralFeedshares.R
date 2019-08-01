#' calcCentralFeedshares
#' @description 
#' Calculates future central feed shares for all livestock categories 
#' based on the results of a non-linear regression between historical 
#' central feed shares and livestock productivity and using Koeppen-
#' Geiger climate zones
#'
#'
#' @return Central feed shares and weights as list of two MAgPIE-objects
#' @author Isabelle Weindl, Benjamin Bodirsky, Stephen Wirth, Jan Philipp Dietrich
#'
#' @importFrom magclass collapseNames
#' @examples 
#' \dontrun{ 
#' calcOutput("CentralFeedshares")
#' 
#' }
calcCentralFeedshares <- function(){
  
  #1. Koeppen input berechnen z
  #2. feed share regression coefficients a,b
  #3. livestock productivity x
  koeppen <- readSource("Koeppen") # read Koeppen
  map <- toolMappingFile(type="sectoral", readcsv=T, name="KoeppenFeedbasketsmapping.csv") # read mapping to Climate regions
  map$Koeppen <- paste0("kg_p_", map$Koeppen)
  koeppen <- koeppen[,,map$Koeppen]
  koeppen[is.na(koeppen)] <- 0 # set NAs to 0

  climatezones <- toolAggregate(koeppen, map, from=1, to=2, dim=3) #Aggregate to climate zones
  climkg13 <- dimSums(climatezones[,,c("temp", "trop")], dim=3) # Sum Climatezones to groups temp#trop
  climkg4 <-  climatezones[,,"cold"] # only cold climate
  
  #calc livst productivity
  lvst_prod <- calcOutput("LivestockProductivity", aggregate = FALSE)
  
  #read regression coefficients for central feed shares
  feed_shr_regr <- readSource("FeedShareReg")
  
  # specify systems for which regressions exist (exluding sys_hen and sys_chicken)
  systems <- c("sys_beef","sys_dairy","sys_pig") 
  lvst_prod <- lvst_prod[,,systems]
  feed_shr_regr <- feed_shr_regr[,,systems]
  
  # set climate-specific factor for different production systems
  climk <- climkg13[,,rep(1,3)]
  getNames(climk) <- systems
  climk[,,"sys_pig"] <- climkg4
  
   func=function(x,a,b,z){
    # x : stock or producer yield
    # a: FeedShare regression coefficient
    # b: FeedShare regression coefficient
    # z: aggregated climate-specific factor for each Magpie region
    out<- z*(1-((x*a)^3/(0.1 + (x*a)^3))) + (1-z)*(1-((x*b)^3/(0.1 + (x*b)^3)))
    return(collapseNames(out))
    #difference in region of 1e-15
   }
   
   #calculate feedshares for livestock commodities
   out_shr <- func(lvst_prod, feed_shr_regr[,,"A"], feed_shr_regr[,,"B"], climk)

   #use livestock production as weight
   kl <- c("livst_pig", "livst_rum", "livst_milk")
   massbalance<-calcOutput("FAOmassbalance_pre",aggregate = F)
   weight <- collapseNames(massbalance[,,kl][,,"dm"][,,"production"])
   
   mapping<-data.frame(
     kl=c( "livst_pig","livst_rum","livst_milk"),
     sys=c("sys_pig","sys_beef","sys_dairy" ),
     stringsAsFactors = FALSE)
   
   weight <- rename_dimnames(weight, dim = 3,query = mapping,from = "kl", to="sys")
   weight <- toolHoldConstantBeyondEnd(weight)

   return(list(x=out_shr,
               weight=weight,
               unit="-",
               description="Central feed shares for dairy cattle, beef cattle and pigs",
               min=0,
               max=1
   ))
  
}