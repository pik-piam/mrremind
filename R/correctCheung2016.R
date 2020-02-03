#' Correct FAO climate impact data from Cheung et al 2016 
#' 
#' Correct magpie objects
#' @param subtype "Area" data subtype. Areas in square km for each Large Marine Ecosystem obtained from Seaaroundus.org
#' "PrimProdinmgCday" data subtype. Primary Production in mg C day^-1 for each Large Marine Ecosystem obtained from Seaaroundus.org
#' "Degrees" data subtype. relative change to fishery production in degree1p5,degree2p5,degree3p5 scenarios. obtained from Cheung et al 2016
#' @return magpie object of the corrected Cheung et al 2016 data and converted x_PrimProdinmgCday into tCyr^(-1)^km2^(-1)
#' @author Jasmin Wehner 
#' @seealso \code{\link{readSource}}
#' @importFrom magclass new.magpie mbind getYears 
#' @importFrom madrat readSource  
#' @export
correctCheung2016 <- function(subtype) {
  
  if (subtype == "Area"){ # Reference Year (e.g. BAU, 2010)
    x_Area <- readCheung2016(subtype = "Area")
    getYears(x_Area) <- "y2100"
    
    #splitting up Large Marine Ecosystems where allocation to Major Fishing Area is ambigous. larger Large Marine 
    #Ecosystems are inserted in the original mapping file, smaller Large Marine Ecosystems are added here.
    #1) Agulhaus Current 
    #40% to Atlantic Southwest; 60% to Indian Ocean Western
    x <- new.magpie(cells_and_regions = "GLO",years = "y2100",
                    names = "Atlantic Southwest.Agulhas Current2", 
                    fill=collapseNames(x_Area[,,"Agulhas Current"]*0.4))
    x_Area[,,"Agulhas Current"] <- x_Area[,,"Agulhas Current"]*0.6
    x_Area <- mbind(x_Area, x)
    #x_Area <- magpie_expand(x,x_Area)
    #2) SE Australian Shelf
    #80% to Pacific Indian Ocean Eastern 20% to Pacific Southwest
    x2 <- new.magpie(cells_and_regions = "GLO",years = "y2100",
                     names = "Pacific Southwest.SE Australian Shelf2",
                     fill=collapseNames(x_Area[,,"SE Australian Shelf"]*0.2)) 
    x_Area[,,"SE Australian Shelf"] <- x_Area[,,"SE Australian Shelf"]*0.8
    x_Area <- mbind(x_Area,x2)
    #3) Pacific Central-American
    #80% to Pacific Eastern Central and 20% to Pacific Southeast
    x3 <- new.magpie(cells_and_regions = "GLO",years = "y2100",
                     names = "Pacific Southeast.Pacific Central American2",
                     fill=collapseNames(x_Area[,,"Pacific Central American"]*0.2))
    x_Area[,,"Pacific Central American"] <- x_Area[,,"Pacific Central American"]*0.8
    x_Area <- mbind(x_Area, x3)
    #4) East-Central Australian Shelf
    #80% to Pacific Southwest and 20% to Pacific Western Central
    x4 <- new.magpie(cells_and_regions = "GLO",years = "y2100",
                     names = "Pacific Western Central.East Central Australian Shelf2",
                     fill=collapseNames(x_Area[,,"East Central Australian Shelf"]*0.2))
    x_Area[,,"East Central Australian Shelf"] <- x_Area[,,"East Central Australian Shelf"]*0.8
    x_Area <- mbind(x_Area, x4)
    return(x_Area)
    
  } else if(subtype == "PrimProdinmgCday"){ 
    x_PrimProdinmgCday <- readCheung2016(subtype = "PrimProdinmgCday")
    getYears(x_PrimProdinmgCday) <- "y2100"
    
    LME_extension <- c("Atlantic Southwest.Agulhas Current","Pacific Southwest.SE Australian Shelf",
                       "Pacific Southeast.Pacific Central American",
                       "Pacific Western Central.East Central Australian Shelf")
    
    for(i in LME_extension){
      i <- paste0(i,"2")
      x <- new.magpie(cells_and_regions = "GLO", years = "y2100",
                      names = i)
      x_PrimProdinmgCday <- mbind(x_PrimProdinmgCday,x)
    }
    
    x_PrimProdinmgCday[,,"Agulhas Current2"] <- x_PrimProdinmgCday[,,"Agulhas Current"]
    x_PrimProdinmgCday[,,"SE Australian Shelf2"] <- x_PrimProdinmgCday[,,"SE Australian Shelf"]
    x_PrimProdinmgCday[,,"Pacific Central American2"] <- x_PrimProdinmgCday[,,"Pacific Central American"]
    x_PrimProdinmgCday[,,"East Central Australian Shelf2"] <- x_PrimProdinmgCday[,,"East Central Australian Shelf"]

    
    #Conversion from mg C day^(-1) m2^(-1) to  tCyr^(-1)^km2^(-1)
    #10^-9 is for mg to t, 10^6 is for  m2 to km2 
    x_PrimProdintCyrkm2 <- x_PrimProdinmgCday * (10^-9) * 365 * 10^6
    

   
    return(x_PrimProdintCyrkm2)
    
  } else if(subtype == "Degrees"){
    x_Degrees <- readCheung2016(subtype = "Degrees")
    
    getYears(x_Degrees) <- "y2100"
    x_Degrees <- x_Degrees*10^-2
    
    #filling missing values for respective Large Marine Ecosystems
    LMarineEcosystems <- c("Beaufort Sea", "West Bering Sea", "Northern Bering Chukchi Seas")
    for(i in LMarineEcosystems){
      for(j in getNames(x_Degrees, dim=3)){
        x_Degrees[,,i][,,j] <- mean(x_Degrees[,,j])
      }
    }
    
    LME_extension <- c("Atlantic Southwest.Agulhas Current","Pacific Southwest.SE Australian Shelf",
                       "Pacific Southeast.Pacific Central American",
                       "Pacific Western Central.East Central Australian Shelf")
    
    for(i in LME_extension){
      i <- paste0(i,"2")
      x <- new.magpie(cells_and_regions = "GLO", years = "y2100",
                      names = paste(i, sep = ".",getNames(x_Degrees, dim=3)))
      x_Degrees <- mbind(x_Degrees,x)
      }
    
    x_Degrees[,,"Agulhas Current2"] <- x_Degrees[,,"Agulhas Current"]
    x_Degrees[,,"SE Australian Shelf2"] <- x_Degrees[,,"SE Australian Shelf"]
    x_Degrees[,,"Pacific Central American2"] <- x_Degrees[,,"Pacific Central American"]
    x_Degrees[,,"East Central Australian Shelf2"] <- x_Degrees[,,"East Central Australian Shelf"]
    
    

    return(x_Degrees)
  }
 
}
