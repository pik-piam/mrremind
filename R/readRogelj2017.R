#' Reads NDC policy database from Rogelj  et al. 2017 with capacity, emission, and share targets
#' @description Reads excel sheet (supplementary sheet from Rogelj et al., 2017 ) with NDC (Nationally Determined Contributions) 
#' data on different policy targets (capacity, emission, and share targets) with different variations
#' @details Country name is ISO coded. Capacity/Additional Capacity targets are in GW. Generation/Production targets are in GWh.
#' @return  magpie object 
#' @author  Aman Malik and Christoph Bertram
#' @param subtype Capacity Generation Emissions Share
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select
#' @importFrom gdata duplicated2


readRogelj2017 <- function(subtype){
  
  if (length(grep("2018",subtype))!=0) {
    NDC_file <- "NDC_2018.xlsx"
  } else if (length(grep("2021",subtype))!=0) {
    NDC_file <- "NDC_2021.xlsx"
  }
  
  if (subtype == "COCapacity"| subtype =="UNCapacity") {
   # Capacity/Additional Capacity targets are in GW. Generation/Production targets are in GWh.
    NDC <- read_excel("NDC_2018.xlsx", sheet = "Capacity_target",
                      col_types = c("text", "skip", "numeric",
                      "text", "text", "numeric", "numeric","numeric", "numeric", "numeric"))

    x <- as.magpie(NDC,spatial=1,temporal=2,datacol=3)
  }
 
  if (substr(subtype,1,9) == "Emissions"){
    input <- read_xlsx(NDC_file, sheet="Emissions",skip = 3, na = c("?",""),)  
    # select the relevant columns to work upon
    input2 <- select(input,2,7:14)
    # rename columns
    colnames(input2) <- c("ISO_Code","Reference_Year","BAU/Reference_emissions_in_MtCO2e","Target_Year","Type","Unconditional","Conditional","Uncond2","Cond2")
    #if no entry in first two quantitative columns, use data from 3rd and 4th
    input2[is.na(input2$Unconditional),]$Unconditional <- input2[is.na(input2$Unconditional),]$Uncond2 
    input2[is.na(input2$Conditional),]$Conditional <- input2[is.na(input2$Conditional),]$Cond2 
    #drop extra columns
    input2$Uncond2 <- NULL
    input2$Cond2 <- NULL
    # Take higher values of a emission target with a range of values; convert percentage to decimal
    # input2$Unconditional <- as.numeric(sub("%", "", sub("?","",sub(".*;", "", input2$Unconditional))))
    # input2$Conditional <- as.numeric(sub("%", "", sub("?","",sub(".*;", "", input2$Conditional))))  
    # gathering data to make it tidy
    # input2 <- input2%>%
    # gather("Unconditional","Conditional", key = "Conditionality",value = "Reduction" )
    
    #fill up conditional/unconditional with respective counterpart
    input2[is.na(input2$Conditional),]$Conditional <- input2[is.na(input2$Conditional),]$Unconditional
    
    #in case a country has two or more types of targets, use absolute targets
    input2 <- input2[!(input2$ISO_Code %in% input2[duplicated(input2$ISO_Code),]$ISO_Code & input2$Type=="GHG"),]
    
    # in case a country has two target years for an absolute-type target, use the later one.
    input3 <- input2[duplicated2(input2$ISO_Code),]
    input3 <- input3[input3$Target_Year==min(input3$Target_Year),]
    input2 <- input2[!(input2$ISO_Code==input3$ISO_Code & input2$Target_Year==input3$Target_Year),]
    
    # Dividing all data columns in separate magpie objects
    if (length(grep("Emissions_Ref",subtype))!=0){ # Reference Year (e.g. BAU, 2010)
      x <- as.magpie(input2[,seq(1,2)],datacol = 2) # See Warning message:  In .duplicates_check(coord) : 
      # Duplicate entries found, only the last entry will be used (duplicate entries: COG|NOTIME|Reference_Year)!
    } else if(length(grep("Emissions_Emi",subtype))!=0){ # Emissions in reference year or BAU
      x <- as.magpie(input2[,c(1,3)],datacol = 2)
    } else if(length(grep("Emissions_Tar",subtype))!=0){# Target Year (2035 or 2030)
      x <- as.magpie(input2[,c(1,4)],datacol = 2)
    } else if(length(grep("Emissions_Typ",subtype))!=0){ # Type of Emission reduction effort
      x <- as.magpie(input2[,c(1,5)],datacol = 2)
    } else if(length(grep("Emissions_Red_unc",subtype))!=0){ # Unconditional Emission reduction
      x <- as.magpie(input2[,c(1,6)],datacol = 2)
    } else if(length(grep("Emissions_Red_con",subtype))!=0){ # Conditional emission reductions
      x <- as.magpie(input2[,c(1,7)],datacol = 2)
    }
    
  }
  
  return(x)
}
