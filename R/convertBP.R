
#' Disaggregates and cleans BP data.
#' @param x MAgPIE object to be converted
#' @param subtype Either "Capacity" or "Generation"
#' @description Disaggregates historical - capacity, generation, and production data.
#' @return A magpie object with historical electricity capacities (in MW for Wind, Solar, and Geothermal),
#' Generation (in TWh for Nuclear, Hydro, Wind, Solar, and Geo Biomass), AND
#' Production (in EJ for Coal, Oil, and Gas, additionally in tonnes for coal)
#' @author Aman Malik
#' @importFrom dplyr filter
#' @importFrom utils write.csv2




convertBP <- function(x,subtype){ 
  Region_name <- NULL
  ISO_code <- NULL
 
    if(subtype == "Capacity"){
    
    # Regions to be disaggregated 
    origReg <- c("Other Europe", "Other Middle East","Other Africa",
                 "Other S & Cent America","Other Asia Pacific","Other CIS")
    # Reading in file with mapping of 209 countries with respective BP regions
    mapping_capacity <- read.csv("BPmappingall.csv",sep = ";",colClasses = "character")
    
    #Substituting certain country names 
    getRegions(x) <- gsub("\\bUS\\b","USA",getRegions(x))
    getRegions(x) <- gsub("France\\s\\(Guadeloupe\\)","Guadeloupe",getRegions(x))
    getRegions(x) <- gsub("Russia\\s\\(Kamchatka\\)","Russia",getRegions(x))
    
    # Adding country Portugal (The Azores) to Portugal
    # x1 <- x["Portugal (The Azores)",,]
    # getRegions(x1) <- c("Portugal") # renaming to Portugal
    # x["Portugal",,] <- x1["Portugal",,] + x1
    # x <- x["Portugal (The Azores)",,,invert = TRUE] # removing Portugal (The Azores)

    # Step 1:Grouping countries according to their region.
    countries_africa <- mapping_capacity$ISO3.code[mapping_capacity$Region_name=="Africa"]
    countries_asia <- mapping_capacity$ISO3.code[mapping_capacity$Region_name=="Asia Pacific"]
    countries_middle_east <- mapping_capacity$ISO3.code[mapping_capacity$Region_name=="Middle East"]
    countries_sc_america <- mapping_capacity$ISO3.code[mapping_capacity$Region_name=="S & C America"]
    countries_europe <- mapping_capacity$ISO3.code[mapping_capacity$Region_name=="Europe"]
    countries_cis <- mapping_capacity$ISO3.code[mapping_capacity$Region_name=="CIS"]
    # Step 2: removing countries where capacity = NA, not in the original Solar database
    na_countries_solar <- getRegions(x)[is.na(x[,2019,"Solar"])]
    # All countries with positive value of capacity in 2019 and not in origReg
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
    other_europe <- data.frame(Region_name="Other Europe",ISO_code=other_europe)
    other_cis <- setdiff(countries_cis,country2iso_solar)
    other_cis <- data.frame(Region_name="Other CIS",ISO_code=other_cis)
    
    # Combining all countries (ISO coded) "others" in one file  
    mainmappingfile <- rbind(other_cis,other_africa,other_asia,other_middle_east,other_sc_america,other_europe)
    mainmappingfile <- filter(mainmappingfile,ISO_code!="SUN")
    write.csv2(mainmappingfile,file = "BPmapping2.csv",row.names=FALSE,quote = F)
    
    # Step 4: Downscaling all "Other_*" into respective countries
    PE <- calcOutput("PE",aggregate = FALSE)[mainmappingfile$ISO_code,2016,"PE (EJ/yr)"]
    output_solar <- toolAggregate(x[origReg,,"Solar"],rel = "BPmapping2.csv",weight = PE)
    output_solar[is.na(output_solar)] <- 0
    output_solar <- toolCountryFill(output_solar, fill=0)
    
    # For Wind
    
    # countries with NA for wind
    na_countries_wind <- getRegions(x)[!is.na(x[,2019,"Wind"])]
    country2iso_wind <- toolCountry2isocode(getRegions(x[c(origReg,na_countries_wind),,,invert = TRUE]))
    
    # countries in other.... are different than that for solar
    other_africa <- setdiff(countries_africa,country2iso_wind)
    other_africa <- data.frame(Region_name=c("Other Africa"),ISO_code=other_africa)
    other_asia <- setdiff(countries_asia,country2iso_wind)
    other_asia <- data.frame(Region_name=c("Other Asia Pacific"),ISO_code=other_asia)
    other_middle_east <- setdiff(countries_middle_east,country2iso_wind)
    other_middle_east <- data.frame(Region_name=c("Other Middle East"),ISO_code=other_middle_east)
    other_sc_america <- setdiff(countries_sc_america,country2iso_wind)
    other_sc_america <- data.frame(Region_name=c("Other S & Cent America"),ISO_code=other_sc_america)
    other_europe <- setdiff(countries_europe,country2iso_wind)
    other_europe <- data.frame(Region_name=c("Other Europe"),ISO_code=other_europe)
    other_cis <- setdiff(countries_cis,country2iso_wind)
    other_cis <- data.frame(Region_name="Other CIS",ISO_code=other_cis)
    
    mainmappingfile <- rbind(other_cis,other_africa,other_asia,other_middle_east,other_sc_america,other_europe)
    mainmappingfile <- filter(mainmappingfile,ISO_code!="SUN")
    write.csv2(mainmappingfile,file = "BPmapping2.csv",row.names=FALSE,quote = F)
    
    # Step 4: Downscaling all "Other_*" into respective countries
    
    PE <- calcOutput("PE",aggregate = FALSE)[mainmappingfile$ISO_code,2016,"PE (EJ/yr)"]
    output_wind <- toolAggregate(x[origReg,,"Wind"],rel = "BPmapping2.csv",weight = PE)
    # output_wind[] <- x[]
    output_wind <- toolCountryFill(output_wind, fill=0)
    
    
    # Step5: Removing others and adding 
    x <- x[origReg,,invert=TRUE]
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    x <- toolCountryFill(x, fill=0)
    # set all NA to 0
    x[is.na(x)] <- 0
    
    x[,,"Solar"] <- x[,,"Solar"] + output_solar
    x[,,"Wind"] <- x[,,"Wind"] + output_wind
  }
  
  if(subtype == "Generation"){
    # IMPORTANT NOTE: Generation Data has not been disaggregated for USSR before 1991.
    
    origReg <- c("Other Europe", "Other Middle East","Other Africa",
                 "Other S & Cent America","Other Asia Pacific","Other CIS")
    getRegions(x) <- gsub("\\bUS\\b","USA",getRegions(x))
    getRegions(x) <- gsub(pattern = "China Hong Kong SAR","Hong Kong",x = getRegions(x))
    other_africas <- c("Other Northern Africa","Other Southern Africa","Middle Africa","Eastern Africa","Western Africa")
    x["Other Africa",,] <- dimSums(x[c(other_africas,"Other Africa"),,],na.rm = T,dim = 1)
    x <- x[other_africas,,invert=T]
    
    other_samicas <- c("Other South America","Other Caribbean","Central America")
    x["Other S & Cent America",,] <- dimSums(x[c(other_samicas,"Other S & Cent America")],dim=1,na.rm = T)
    x <- x[other_samicas,,invert=T]

  
    # Downscaled regions in ISO3
    read_mapping_file <- read.csv2("BPmapping.csv")
    PE <- calcOutput("PE",aggregate = FALSE)[unique(read_mapping_file$ISO.Code),2016,"PE (EJ/yr)"]
    x_row <- toolAggregate(x[origReg,,],rel = "BPmapping.csv",weight = PE)
    x_row <- toolCountryFill(x_row,fill = 0)
    x_row[is.na(x_row)] <- 0
    
    x <- x[origReg,,invert=TRUE]
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    x <- toolCountryFill(x,fill = 0)
    x[is.na(x)] <- 0
    
    # Combine the two objects containing normal and disaggregated data
    x <- x + x_row
  }
  
  if (subtype=="Production")
  {
    origReg <- c("Other Europe", "Other Middle East","Other Africa",
                 "Other S & Cent America","Other Asia Pacific","Other CIS")
    
    x <- x[c("OPEC","Non-OPEC"),,invert=T]
    getRegions(x) <- gsub("\\bUS\\b","USA",getRegions(x))
    
    read_mapping_file <- read.csv2("BPmapping.csv")
    PE <- calcOutput("PE",aggregate = FALSE)[read_mapping_file$ISO.Code,2016,"PE (EJ/yr)"]
    x_row <- toolAggregate(x = x[origReg,,],rel = "BPmapping.csv",weight = PE)
    x_row <- toolCountryFill(x_row,fill = 0)
    x_row[is.na(x_row)] <- 0
    
    x <- x[origReg,,invert=TRUE]
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    x <- toolCountryFill(x,fill = 0)
    x[is.na(x)] <- 0
    
    # Combine the two objects containing normal and disaggregated data
    x <- x + x_row
    
    
  }
  
  return (x)
  
}








