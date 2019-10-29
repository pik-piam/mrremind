#' @title convertGTAP
#' @description Converts GTAP data to fit to the common country list. Weighting is done by using the Imports and Exports from FAO
#'
#'
#' @param x MAgPIE object contains GTAP data 
#' @param subtype The GTAP subtype: VIWS, VIMS VXWD, VXMD, VOA, VOM
#' @return Converted GTAP Data
#' @author Xiaoxi Wang
#' @examples
#'
#'   \dontrun{
#'     x <- ReadSource("GTAP", "GTAP7_VIMS")
#'   }
#' @importFrom madrat toolAggregate
#' @importFrom magclass as.magpie magpiesort
convertGTAP <- function(x,subtype) {
  
  x <- magpiesort(x)
  gtap <-  paste0(gsub("(?<=\\d)\\w{1,}", "", subtype, perl = T))
  subtype <- unlist(strsplit(subtype,"_"))[[2]] 
  
  fao <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, 2005, "dm"])

  region_mapping <- toolMappingFile("regional",paste0(gtap,"Mapping2016.csv"),readcsv = TRUE,where = "moinput")
  sector_mapping <- toolMappingFile("sectoral","mappingGTAPMAgPIETrade.csv",readcsv = TRUE,where ="moinput")
  sector_mapping <- sector_mapping[which(sector_mapping$gtap !="xxx" & sector_mapping$magpie !="zzz"),]
  if (subtype %in% c("VIWS","VIMS","VXWD","VXMD")){
    fao_import <- collapseNames(fao[, 2005, "import"])
    fao_export <- collapseNames(fao[, 2005, "export"])
    if (subtype %in% c("VXWD","VXMD")){
      w1 <- toolAggregate(fao_export,rel=region_mapping,from ="Country.code", to ="Region.code",dim=1,partrel = TRUE)
      w1 <- toolAggregate(w1,rel=sector_mapping,from ="magpie",to="gtap",dim=3,partrel = TRUE)
    
      w2 <- toolAggregate(fao_import,rel=region_mapping,from ="Country.code", to ="Region.code",dim=1,partrel = TRUE)
      w2 <- toolAggregate(w2,rel=sector_mapping,from ="magpie",to="gtap",dim=3,partrel = TRUE)
    
      w3 <- toolAggregate(fao_export,rel=sector_mapping,from ="magpie", to ="gtap",dim=3,partrel = TRUE)
    
      out <- toolAggregate(x[,,getNames(w1)],rel=region_mapping,from ="Region.code", to ="Country.code",dim=1,weight =w1)
      out <- as.magpie(aperm(unwrap(out),c(3,2,1,4)),spatial =1, temporal =2)
      out <- toolAggregate(out[,,getNames(w2)],rel=region_mapping,from ="Region.code", to ="Country.code",dim=1,weight =w2)
      out <- as.magpie(aperm(unwrap(out),c(3,2,1,4)),spatial =1, temporal =2)
      out <- toolAggregate(out,rel = sector_mapping, from = "gtap",to ="magpie", dim=3.2, weight = w3 [getRegions(out),,],partrel = TRUE )
      } else if (subtype  %in% c("VIWS","VIMS")){
      w1 <- toolAggregate(fao_import,rel=region_mapping,from ="Country.code", to ="Region.code",dim=1,partrel = TRUE)
      w1 <- toolAggregate(w1,rel=sector_mapping,from ="magpie",to="gtap",dim=3,partrel = TRUE)
    
      w2 <- toolAggregate(fao_export,rel=region_mapping,from ="Country.code", to ="Region.code",dim=1,partrel = TRUE)
      w2 <- toolAggregate(w2,rel=sector_mapping,from ="magpie",to="gtap",dim=3,partrel = TRUE)
    
      w3 <- toolAggregate(fao_import,rel=sector_mapping,from ="magpie", to ="gtap",dim=3,partrel = TRUE)
    
      out <- toolAggregate(x[,,getNames(w1)],rel=region_mapping,from ="Region.code", to ="Country.code",dim=1,weight =w1)
      out <- as.magpie(aperm(unwrap(out),c(3,2,1,4)),spatial =1, temporal =2)
      out <- toolAggregate(out[,,getNames(w2)],rel=region_mapping,from ="Region.code", to ="Country.code",dim=1,weight =w2)
      out <- as.magpie(aperm(unwrap(out),c(3,2,1,4)),spatial =1, temporal =2)
      out <- toolAggregate(out,rel = sector_mapping, from = "gtap",to ="magpie", dim=3.2, weight = w3 [getRegions(out),,],partrel = TRUE )
      }
    }else if (subtype %in% c("VOM","VOA")){
      fao_production <- collapseNames(fao[,,"production"])
      w1 <- toolAggregate(fao_production,rel=region_mapping,from ="Country.code", to ="Region.code",dim=1,partrel = TRUE)
      w1 <- toolAggregate(w1,rel=sector_mapping,from ="magpie",to="gtap",dim=3,partrel = TRUE)
    
      w2 <- toolAggregate(fao_production,rel=sector_mapping,from ="magpie", to ="gtap",dim=3,partrel = TRUE)
    
      out <- toolAggregate(x[,,getNames(w1)],rel=region_mapping,from ="Region.code", to ="Country.code",dim=1,weight =w1)
      out <- toolAggregate(out,rel = sector_mapping, from = "gtap",to ="magpie", dim=3, weight = w2[getRegions(out),,],partrel = TRUE )
    }else (stop("Not supported by current convertGTAP funtion, please set convert to FALSE!"))
  out <- toolCountryFill(out,0)
  return(out)
}