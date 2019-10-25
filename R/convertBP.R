
#' Disaggregates and cleans BP data.
#' @description    
#' @param x MAgPIE object to be converted
#' @param subtype Either "Capacity" or "Generation"
#' @description Disagregates istorical capacity and generation data.
#' @return A magpie object of the BP country disaggregated data  with historical electricity capacities 
#' (in MW for Wind, Solar, and Geothermal for 1997-2012) AND Generation
#'  (in TWh for Nuclear, Hydro, Wind, Solar, Other Renewables, and Geo Biomass for 1971-2012) 
#' @author Aman Malik
#' @importFrom dplyr filter
#' @importFrom utils write.csv2




convertBP <- function(x,subtype){ 
  Region_name <- NULL
  ISO_code <- NULL
 
    if(subtype == "Capacity"){
    
    # Regions to be disaggregated 
    origReg <- c("Other Europe & Eurasia", "Other Middle East","Other Africa",
                 "Other S & Cent America","Other Asia Pacific")
    # Reading in file with mapping of 209 countries with respective BP regions
    mapping_capacity <- read.csv("BPmappingall.csv",sep = ";",colClasses = "character")
    
    #Substituing certain country names 
    getRegions(x) <- gsub("\\bUS\\b","USA",getRegions(x))
    getRegions(x) <- gsub("France\\s\\(Guadeloupe\\)","Guadeloupe",getRegions(x))
    getRegions(x) <- gsub("Russia\\s\\(Kamchatka\\)","Russia",getRegions(x))
    
    # Adding country Portugal (The Azores) to Portugal
    x1 <- x["Portugal (The Azores)",,]
    getRegions(x1) <- c("Portugal") # renaming to Portugal
    x["Portugal",,] <- x1["Portugal",,] + x1
    x <- x["Portugal (The Azores)",,,invert = TRUE] # removing Portugal (The Azores)

    # Step 1:Grouping countries according to their region.
    countries_africa <- mapping_capacity$ISO3.code[mapping_capacity$Region_name=="Africa"]
    countries_asia <- mapping_capacity$ISO3.code[mapping_capacity$Region_name=="Asia Pacific"]
    countries_middle_east <- mapping_capacity$ISO3.code[mapping_capacity$Region_name=="Middle East"]
    countries_sc_america <- mapping_capacity$ISO3.code[mapping_capacity$Region_name=="S & C America"]
    countries_europe <- mapping_capacity$ISO3.code[mapping_capacity$Region_name=="Europe & Eurasia"]
    
    # Step 2: removing countries where capacity = NA, not in the original Solar database
    na_countries_solar <- getRegions(x)[is.na(x[,2016,"Solar"])]
    # All countries with positive value of capacity in 2016 and not in origReg
    country2iso_solar <- toolCountry2isocode(getRegions(x[c(origReg,na_countries_solar),,,invert = TRUE]))
    
    # Step 3: Finding all countries in "Other_*"
    other_africa <- setdiff(countries_africa,country2iso_solar)
    other_africa <- data.frame(Region_name="Other Africa",ISO_code=other_africa)
    other_asia <- setdiff(countries_asia,country2iso_solar)
    other_asia <- data.frame(Region_name="Other Asia Pacific",ISO_code=other_asia)
    other_middle_east <- setdiff(countries_middle_east,country2iso_solar)
    other_middle_east <- data.frame(Region_name="Other Middle East",ISO_code=other_middle_east)
    other_sc_america <- setdiff(countries_sc_america,country2iso_solar)
    other_sc_america <- data.frame(Region_name="Other S & Cent America",ISO_code=other_sc_america)
    other_europe <- setdiff(countries_europe,country2iso_solar)
    other_europe <- data.frame(Region_name="Other Europe & Eurasia",ISO_code=other_europe)
    
    
    # Combining all countries (ISO coded) "others" in one file  
    mainmappingfile <- rbind(other_africa,other_asia,other_middle_east,other_sc_america,other_europe)
    mainmappingfile <- filter(mainmappingfile,ISO_code!="SUN")
    write.csv2(mainmappingfile,file = "BPmapping2.csv",row.names=FALSE)
    
    # Step 4: Downscaling all "Other_*" into respective countries
    PE <- calcOutput("PE",aggregate = FALSE)[mainmappingfile$ISO_code,1997:2015,"PE (EJ/yr)"]
    output_solar <- toolAggregate(x[origReg,1997:2015,"Solar"],"BPmapping2.csv",weight = PE)
    # a <- output_solar[,2012,]
    output_solar <- toolCountryFill(output_solar, fill=0)
    
    # For Wind
    
    # countries with NA for wind
    na_countries_wind <- getRegions(x)[!is.na(x[,2016,"Wind"])]
    country2iso_wind <- toolCountry2isocode(getRegions(x[c(origReg,na_countries_wind),,,invert = TRUE]))
    
    other_africa <- setdiff(countries_africa,country2iso_wind)
    other_africa <- data.frame(Region_name=c("Other Africa"),ISO_code=other_africa)
    other_asia <- setdiff(countries_asia,country2iso_wind)
    other_asia <- data.frame(Region_name=c("Other Asia Pacific"),ISO_code=other_asia)
    other_middle_east <- setdiff(countries_middle_east,country2iso_wind)
    other_middle_east <- data.frame(Region_name=c("Other Middle East"),ISO_code=other_middle_east)
    other_sc_america <- setdiff(countries_sc_america,country2iso_wind)
    other_sc_america <- data.frame(Region_name=c("Other S & Cent America"),ISO_code=other_sc_america)
    other_europe <- setdiff(countries_europe,country2iso_wind)
    other_europe <- data.frame(Region_name=c("Other Europe & Eurasia"),ISO_code=other_europe)
    
    mainmappingfile <- rbind(other_africa,other_asia,other_middle_east,other_sc_america,other_europe)
    mainmappingfile <- filter(mainmappingfile,ISO_code!="SUN")
    write.csv2(mainmappingfile,file = "BPmapping2.csv",row.names=FALSE)
    
    # Step 4: Downscaling all "Other_*" into respective countries
    
    PE <- calcOutput("PE",aggregate = FALSE)[mainmappingfile$ISO_code,1997:2015,"PE (EJ/yr)"]
    output_wind <- toolAggregate(x[origReg,1997:2015,"Wind"],"BPmapping2.csv",weight = PE)
    # output_wind[] <- x[]
    output_wind <- toolCountryFill(output_wind, fill=0)
    
    
    # Step5: Removing others and adding 
    x <- x[origReg,,invert=TRUE]
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    x <- toolCountryFill(x, fill=0)
    # set all NA to 0
    x[is.na(x)] <- 0
    
    x[,1997:2015,"Solar"] <- x[,1997:2015,"Solar"] + output_solar
    x[,1997:2015,"Wind"] <- x[,1997:2015,"Wind"] + output_wind
  }
  
  if(subtype == "Generation"){
    # IMPORTANT NOTE: Generation Data has not been disaggregated for USSR before 1991.
    
    origReg <- c("Other Europe & Eurasia", "Other Middle East","Other Africa",
                 "Other S & Cent America","Other Asia Pacific")
    getRegions(x) <- gsub("\\bUS\\b","USA",getRegions(x))
    # Downscaled regions in ISO3
    read_mapping_file <- read.csv2("BPmapping.csv")
    PE <- calcOutput("PE",aggregate = FALSE)[read_mapping_file$ISO.Code,1971:2015,"PE (EJ/yr)"]
    x_row <- toolAggregate(x[origReg,1971:2015,],"BPmapping.csv",weight = PE)
    x_row <- toolCountryFill(x_row,fill = 0)
    x_row[is.na(x_row)] <- 0
    x <- x[origReg,,invert=TRUE]
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    x <- toolCountryFill(x,fill = 0)
    x[is.na(x)] <- 0
    
    # Combine the two objects containing normal and disaggregated data
    x[,1971:2015,] <- x[,1971:2015,] + x_row
  }
  
  return (x)
  
}








