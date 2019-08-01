#' @title calcFAOForestRelocate
#' @description Calculates the cellular MAgPIE forest and other land area correction based on FAO forestry data and LUH2v2.
#'
#' @param selectyears default on "past"
#' @param track if intermediate results shoul be printed
#' @return List of magpie object with results on cellular level, weight on cellular level, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FAOForestRelocate")
#' }
#' @importFrom magclass setNames
#' @import nleqslv
#' @importFrom nleqslv nleqslv
#' @export

calcFAOForestRelocate <- function(selectyears="past", track=TRUE){
  
  # Load cellular and country data
  countrydata <- calcOutput("LanduseInitialisation",aggregate = FALSE,land="fao", selectyears=selectyears ,cellular=FALSE)
  LUH2v2      <- calcOutput("LanduseInitialisation",aggregate = FALSE,land="new", selectyears=selectyears , cellular=TRUE)
  
  totalarea   <- dimSums(LUH2v2, dim=c(1,3))
  
  mapping     <- toolGetMapping(name="CountryToCellMapping.csv", type="cell") 
  countries   <- sort(unique(mapping$iso))
  
  forests <- c("primforest","secdforest","forestry")
  nature  <- c(forests, "other")
  landuse <- c(nature, "urban", "crop", "past")
   
  # reduce, if nessessary to FAO 
  reduce     <- increase <- round(countrydata - toolCountryFill(toolAggregate(LUH2v2, rel=mapping, from="celliso", to="iso", partrel=TRUE), fill=0),8)
  reduce[reduce>0]     <- 0
  increase[increase<0] <- 0
  
  LUH2v2 <- add_columns(LUH2v2, "to_be_allocated", dim=3.1)
  LUH2v2[,,"to_be_allocated"] <- 0
  
  #grep land areas dependend on vegetation carbon density
  cellvegc_n <- cellvegc <- readSource("LPJml_rev21", subtype="vegc", convert="onlycorrect")[,getYears(countrydata),]
  
  #weight function to determine correct cellweights for area removal
  findweight <- function(p,cellarea,isoreduction,cellweight){
    dimSums(cellarea*(1-(1-cellweight)^p), dim=1)+isoreduction+10^-10
  }
  
  #loop over countries and years
  for(iso in countries){
    for(t in getYears(countrydata)){
      
      #normalized vegetation carbon (with small correction to ensure values between [0,1))
      cellvegc_n[iso,t,] <- cellvegc[iso,t,]/(setNames(max(cellvegc[iso,t,]), NULL)+10^-10)
      
      if(track) print(paste0("### START REALLOCATION of: ",iso," ",t))
      ###########################
      ### Reduction procedure ###
      ###########################
      
      #loop over all land use categories, that has to be reallocated
      for(cat in nature){
        
        #check if area has to be cleared
        if(reduce[iso,t,cat]!=0){
          
          #for other land cell with highest vegc and for all forest categories lowest vegc should be cleared first
          if(cat=="other"){
            cellweight <- cellvegc_n[iso,t,]
          } else {
            cellweight <- (1-10^-16-cellvegc_n[iso,t,])
          }
          
          #check for one cell countries
          if(length(getCells(LUH2v2[iso,t,cat]))==1){
            #trivial case of one cell countries
            remove <- - setCells(reduce[iso,t,cat], nm="GLO")
            if(track) print(paste0("Remove ",remove," Mha from ",cat," for one cell country."))
          } else{
            #determine correct parameter for weights for multiple cell countries (weights below zero indicate an error)
            p      <- nleqslv(1,findweight, cellarea=LUH2v2[iso,t,cat], isoreduction=reduce[iso,t,cat], cellweight=cellweight)$x
            if(p<0) stop(verbosity = 2,paste0("Negative weight of p=",p," for: ",cat," ",iso," ",t))
            remove <- LUH2v2[iso,t,cat] * (1-(1-cellweight)^p)
            if(track) print(paste0("Remove ",dimSums(remove, dim=1)," Mha from ",cat," with p = ",p))
          }
          
          #remove area from cells and put to "to_be_allocated" area
          LUH2v2[iso,t,cat]               <- LUH2v2[iso,t,cat] - remove
          LUH2v2[iso,t,"to_be_allocated"] <- LUH2v2[iso,t,"to_be_allocated"] + remove
          reduce[iso,t,cat] <- 0
        }
      }
      
      ############################
      ### Allocation procedure ###
      ############################
      
      #relocate other land to areas with low vegetation carbon density
      #check if other land has to be filled
      if(increase[iso,t,"other"]!=0){
        
        cellweight <- (1-10^-16-cellvegc_n[iso,t,])
        
        
        #check for one cell countries
        if(length(getCells(LUH2v2[iso,t,"other"]))==1){
          #trivial case of one cell countries
          add    <- setCells(increase[iso,t,cat], nm="GLO")
          if(track) print(paste0("Add ",add," Mha to other for one cell country."))
        } else{
          #determine correct parameter for weights for multiple cell countries (weights below zero indicate an error)
          p      <- nleqslv(1,findweight, cellarea=LUH2v2[iso,t,"to_be_allocated"], isoreduction=-increase[iso,t,"other"], cellweight=cellweight)$x
          if(p<0) stop(verbosity = 2,paste0("Negative weight of p=",p," for: ",cat," ",iso," ",t))
          add    <- LUH2v2[iso,t,"to_be_allocated"] * (1-(1-cellweight)^p)
          if(track) print(paste0("Add ",dimSums(add, dim=1)," Mha to other with p = ",p))
        }
        
        #move area from "to_be_allocated" area to other land
        LUH2v2[iso,t,"other"]           <- LUH2v2[iso,t,"other"] + add
        LUH2v2[iso,t,"to_be_allocated"] <- LUH2v2[iso,t,"to_be_allocated"] - add
        increase[iso,t,"other"]         <- 0
      }
      
      #relocate forest land to remaining "to_be_allocated" area 
      #check if forests has to be filled
      if(any(increase[iso,t,forests]!=0)){
        
        #move area from "to_be_allocated" area to forests
        forests_share         <- increase[iso,t,forests]/setNames(dimSums(increase[iso,t,forests],dim=3),NULL) 
        LUH2v2[iso,t,forests] <- LUH2v2[iso,t,forests] + setCells(forests_share,"GLO") * setNames(LUH2v2[iso,t,"to_be_allocated"], NULL)
        if(track) print(paste0("Add ",dimSums(LUH2v2[iso,t,"to_be_allocated"], dim=1)," Mha to forest areas."))
        
        LUH2v2[iso,t,"to_be_allocated"] <- 0
        increase[iso,t,]                <- 0 
      }
      
      ############################
      ### Check reallocation   ###
      ############################
      
      if(!all(round(dimSums(LUH2v2[iso,t,landuse], dim=1),3)==round(countrydata[iso,t,landuse],3))){
        warning(paste0("Missmatch in data for in ",iso," ",t))
      } else if(track){
        print(paste0("### DONE REALLOCATION of: ",iso," ",t))
      }
    }
  }
  
  out <- LUH2v2[,,landuse]
  
  if(!all( round(dimSums(out, dim=c(1,3)),3) == round(totalarea,3) )){
    vcat(2,"Something went wrong. Missmatch in total land cover area after reallocation.")
  } 
  
  return(list(x=out,
              weight=NULL,
              unit="Mha",
              description="Cellular land use initialisation data for different land pools FAO forest corrected",
              isocountries=FALSE)) 
}