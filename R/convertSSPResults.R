#' @title convertSSPResults
#' @description Disagregates from SSP regions to ISO countries
#'
#' @param x object coming from read function
#' 
#' @return MAgPIE object with ISO countries with all indicators for which disaggregation weight was found
#' @author Abhijeet Mishra, Benjamin Leon Bodirsky, Florian Humpenoeder
#' @seealso
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' readSource("SSPResults",aggregate=TRUE)
#' }
#' 


convertSSPResults<- function(x){
  
  out <- NULL
  
  x=x[,c(2005,2010+(0:9)*10),]
  
  # region-to-ISO-country mapping for regional disaggregation
  mappingFile <- toolMappingFile("regional","regionmappingSSP.csv",error.missing=TRUE,readcsv = T)

  # ---- Population ----
  
  selection <- c("Population (million)")
  
  data<-x[,,selection]
  
  # read in disaggregation weight population
  pop_past<-calcOutput("PopulationPast",aggregate = F)[,"y2010",] #Read about CalcOutput
  pop_past<-setYears(pop_past,NULL)
  
  aggregatedREG <- toolAggregate(data["GLO",,,invert=TRUE], rel = mappingFile,weight =  pop_past, dim = 1, partrel = F, from = "RegionCode", to = "CountryCode")
  
  # no data with NA
  aggregatedREG <- toolCountryFill(aggregatedREG,000)
  out <- mbind(out,aggregatedREG)
  
  # ---- Land ----
  
  selection <-c("Land Cover (million ha)","Land Cover|Built-up Area (million ha)",
                "Land Cover|Cropland (million ha)","Land Cover|Cropland|Energy Crops (million ha)",
                "Land Cover|Forest (million ha)","Land Cover|Forest|Forestry (million ha)",
                "Land Cover|Forest|Forestry|Harvested Area (million ha)","Land Cover|Forest|Natural Forest (million ha)", 
                "Land Cover|Other Arable Land (million ha)","Land Cover|Other Land (million ha)", 
                "Land Cover|Other Natural Land (million ha)","Land Cover|Pasture (million ha)")
  
  weight <- setYears(dimSums(calcOutput("LanduseInitialisation",aggregate = FALSE),dim=3)[,2010,],NULL) #use land area as weight
  
  for (sel in selection) {
    print(sel)
    data <- x[,,sel]
    #weight is needed here. Otherwise all countries belonging to a region get the same value!
    #please replace pop_past with a proper weigt. For instance area.
    aggregatedREG <- toolAggregate(data["GLO",,,invert=TRUE], rel = mappingFile,weight =  weight, dim = 1, partrel = F, from = "RegionCode", to = "CountryCode")
    aggregatedREG <- toolCountryFill(aggregatedREG,000)
    out <- mbind(out,aggregatedREG)
  }
  
  
  # ---- Bioenergy ----
  
  selection<-c("Primary Energy|Biomass|1st Generation (EJ/yr)",
               "Primary Energy|Biomass|Energy Crops (EJ/yr)",
               "Agricultural Demand|Bioenergy|1st generation (million t DM/yr)",
               "Agricultural Demand|Bioenergy|2nd generation (million t DM/yr)")

  weight_bio <- setYears(dimSums(calcOutput("Croparea", sectoral="kcr", physical=TRUE, aggregate = FALSE),dim=3)[,2010,],NULL) #use harvested area as weight
  
  for (sel in selection) {
    print(sel)
    data <- x[,,sel]
    aggregatedREG <- toolAggregate(data["GLO",,,invert=TRUE], rel = mappingFile,weight =  weight_bio, dim = 1, partrel = F, from = "RegionCode", to = "CountryCode")
    aggregatedREG <- toolCountryFill(aggregatedREG,000)
    out <- mbind(out,aggregatedREG)
  }
  
  # ---- CO2 price ----
  
  selection <- c("Price|Carbon (US$2005/t CO2)")
  
  data <- x[,,selection]
  data[is.na(data)]<-0
  aggregatedREG <- toolAggregate(data, rel = mappingFile,weight = NULL, dim = 1, partrel = T, from = "RegionCode", to = "CountryCode")
  aggregatedREG <- toolCountryFill(aggregatedREG,000)
  out <- mbind(out,aggregatedREG)
  
  # ---- Bioenergy price ----
  
  selection <- c("Price|Primary Energy|Biomass (US$2005/GJ)")
  
  data <- x[,,selection]
  aggregatedREG <- toolAggregate(data, rel = mappingFile,weight = NULL, dim = 1, partrel = T, from = "RegionCode", to = "CountryCode")
  aggregatedREG <- toolCountryFill(aggregatedREG,000)
  out <- mbind(out,aggregatedREG)
  
  ### check for NA's
  if(any(is.na(out))){vcat(verbosity=1, "SSP3 results of MESSAGE-GLOBIOM contains NAs which where substituted by 0")}
  out[,c("y2005","y2010"),"MESSAGE-GLOBIOM.Land Cover|Cropland|Energy Crops (million ha)"]<-0
  if(any(is.na(out))){vcat(verbosity=1, "NAs in dataset")}
  
  # ---- Select scenarios ----
  
  subset<-c("SSP1-19-SPA1-V16.IMAGE",
            "SSP1-26-SPA1-V15.IMAGE"  ,
            "SSP1-34-SPA1-V15.IMAGE"  ,
            "SSP1-45-SPA1-V15.IMAGE" ,
            "SSP1-Ref-SPA0-V15.IMAGE",
            
            "SSP1-19-SPA1-V16.REMIND-MAGPIE",
            "SSP1-26-SPA1-V15.REMIND-MAGPIE"  ,
            "SSP1-34-SPA1-V15.REMIND-MAGPIE"  ,
            "SSP1-45-SPA1-V15.REMIND-MAGPIE" ,
            "SSP1-Ref-SPA0-V15.REMIND-MAGPIE",
            
            "SSP2-19-SPA2-V16.MESSAGE-GLOBIOM",
            "SSP2-26-SPA2-V16.MESSAGE-GLOBIOM", 
            "SSP2-34-SPA2-V16.MESSAGE-GLOBIOM",  
            "SSP2-45-SPA2-V16.MESSAGE-GLOBIOM",  
            "SSP2-60-SPA2-V16.MESSAGE-GLOBIOM",  
            "SSP2-Ref-SPA0-V16.MESSAGE-GLOBIOM",
            
            "SSP2-19-SPA2-V16.REMIND-MAGPIE",  
            "SSP2-26-SPA2-V15.REMIND-MAGPIE",    
            "SSP2-34-SPA2-V15.REMIND-MAGPIE",  
            "SSP2-45-SPA2-V15.REMIND-MAGPIE",  
            "SSP2-60-SPA2-V15.REMIND-MAGPIE",  
            "SSP2-Ref-SPA0-V15.REMIND-MAGPIE",
            
            "SSP3-34-SPA3-V15.AIM/CGE",  
            "SSP3-45-SPA3-V15.AIM/CGE",  
            "SSP3-60-SPA3-V15.AIM/CGE",  
            
            "SSP4-26-SPA4-V16.GCAM4",  
            "SSP4-34-SPA4-V16.GCAM4", 
            "SSP4-45-SPA4-V16.GCAM4",  
            "SSP4-60-SPA4-V16.GCAM4", 
            "SSP4-Ref-SPA0-V16.GCAM4", 
            
            "SSP5-19-SPA5-V16.REMIND-MAGPIE",
            "SSP5-26-SPA5-V15.REMIND-MAGPIE",  
            "SSP5-34-SPA5-V15.REMIND-MAGPIE",  
            "SSP5-45-SPA5-V15.REMIND-MAGPIE",  
            "SSP5-60-SPA5-V15.REMIND-MAGPIE",  
            "SSP5-Ref-SPA0-V15.REMIND-MAGPIE")

  out<-out[,,subset]
  
  # ----  Rename scenarios ----
  
  getNames(out) <- gsub("-SPA[0-9]-V1[0-9].","-",getNames(out))
  getNames(out) <- sub("AIM/","AIM-", getNames(out))
  getNames(out) <- paste0("SSPDB-",getNames(out))
  
  names(dimnames(out))<- sub("scenario.", "scenario-", names(dimnames(out)))
  
  
  return(out) 
  
}

