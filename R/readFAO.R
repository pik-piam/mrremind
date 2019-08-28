#' Read FAO
#' 
#' Read in FAO data that has been bulk downloaded from the FAOSTAT website.
#' Files with exception of fodder.csv are aquired from:
#' http://faostat.fao.org/Portals/_Faostat/Downloads/zip_files/
#' 
#' Update 23-Jan-2017 - Added FAO Forestry production and trade data (Abhi)
#' 
#' 
#' @param subtype Type of FAO data that should be read. Available types are:
#' \itemize{ 
#' \item \code{CBCrop}: Commodity Balance Crop (CommodityBalances_Crops_E_All_Data.zip)
#' \item \code{CBLive}: Commoditiy Balance Livestock (CommodityBalances_LivestockFish_E_All_Data.zip)
#' \item \code{Crop}: Production Crops ("Production_Crops_E_All_Data.zip")
#' \item \code{CropProc}: Production Crops Processed ("Production_CropsProcessed_E_All_Data.zip")
#' \item \code{Fbs}: Food Balance Sheet ("FoodBalanceSheets_E_All_Data.zip")
#' \item \code{Fertilizer}: Fertilizer ("Resources_Fertilizers_E_All_Data.zip")
#' \item \code{Fodder}: Fodder (data that has been manually downloaded from the FAOSTAT website as
#' seperate .xls files via a search for "forage" and "fodder" withing
#' Production-Crops. These datasets have been added together to a "Fodder.csv" file)
#' \item \code{FoodSecurity}: Food Security Data ("Food_Security_Data_E_All_Data.zip")
#' \item \code{FSCrop}: Food Supply Crops ("FoodSupply_Crops_E_All_Data.zip")
#' \item \code{FSLive}: Food Supply Livestock ("FoodSupply_LivestockFish_E_All_Data.zip")
#' \item \code{Land}: Land ("Resources_Land_E_All_Data.zip")
#' \item \code{LiveHead}: Production Live Animals ("Production_Livestock_E_All_Data.zip")
#' \item \code{LivePrim}: Production Livestock Primary ("Production_LivestockPrimary_E_All_Data.zip")
#' \item \code{LiveProc}: Production Livestock Processed ("Production_LivestockProcessed_E_All_Data.zip")
#' \item \code{Pop}: Population ("Population_E_All_Data.zip") 
#' \item \code{ForestProdTrade}: Forestry Production and Trade ("Forestry_E_All_Data_(Normalized).zip")
#' \item \code{PricesProducerAnnual}: Producer Prices - Annual ("Prices_E_All_Data.zip")
#' \item \code{PricesProducerAnnualLCU}: Producer Prices - Annual in LCU ("Prices_E_All_Data.zip")
#' \item \code{ValueOfProd}: Value of Agricultural Production ("Value_of_Production_E_All_Data.zip") 
#' }
#' @return FAO data as MAgPIE object
#' @author Ulrich Kreidenweis, Abhijeet Mishra, Mishko Stevanovic
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#'   \dontrun{ a <- readSource("FAO","Crop")
#'   }
#' @importFrom tools file_ext
#' @importFrom data.table fread
#' @importFrom utils unzip
#' @importFrom tools file_path_sans_ext

readFAO <- function(subtype) {
  files <- c(CBCrop="CommodityBalances_Crops_E_All_Data.zip",
             CBLive="CommodityBalances_LivestockFish_E_All_Data.zip",
             Crop="Production_Crops_E_All_Data.zip",
             CropProc="Production_CropsProcessed_E_All_Data.zip",
             # Fbs="FoodBalanceSheets_E_All_Data.zip", #should not be used, use CB and FS or calcFAOharmonized() instead
             Fertilizer="Environment_Fertilizers_E_All_Data.zip",
             Fodder="Fodder.csv",
             FoodSecurity="Food_Security_Data_E_All_Data.zip",
             FSCrop="FoodSupply_Crops_E_All_Data.zip",
             FSLive="FoodSupply_LivestockFish_E_All_Data.zip",
             Land="Resources_Land_E_All_Data.zip",
             LiveHead="Production_Livestock_E_All_Data.zip",
             LivePrim="Production_LivestockPrimary_E_All_Data.zip",
             LiveProc="Production_LivestockProcessed_E_All_Data.zip",
             Pop="Population_E_All_Data.zip",
             PricesProducerAnnual="Prices_E_All_Data.zip",
             
             EmisAgTotal="Emissions_Agriculture_Agriculture_total_E_All_Data.zip",
             EmisAgBurnCropResid="Emissions_Agriculture_Burning_crop_residues_E_All_Data.zip",
             EmisAgBurnSavanna="Emissions_Agriculture_Burning_Savanna_E_All_Data.zip",
             EmisAgCropResid="Emissions_Agriculture_Crop_Residues_E_All_Data.zip",
             EmisAgCultOrgSoil="Emissions_Agriculture_Cultivated_Organic_Soils_E_All_Data.zip",
             EmisAgEnergy="Emissions_Agriculture_Energy_E_All_Data.zip",
             EmisAgEntericFerment="Emissions_Agriculture_Enteric_Fermentation_E_All_Data.zip",
             EmisAgManureSoil="Emissions_Agriculture_Manure_applied_to_soils_E_All_Data.zip", 
             EmisAgManurePasture="Emissions_Agriculture_Manure_left_on_pasture_E_All_Data.zip",
             EmisAgManureManag="Emissions_Agriculture_Manure_Management_E_All_Data.zip",
             EmisAgRiceCult="Emissions_Agriculture_Rice_Cultivation_E_All_Data.zip",
             EmisAgSynthFerti="Emissions_Agriculture_Synthetic_Fertilizers_E_All_Data.zip",
             
             EmisLuBurnBiomass="Emissions_Land_Use_Burning_Biomass_E_All_Data.zip",
             EmisLuCrop="Emissions_Land_Use_Cropland_E_All_Data.zip",
             EmisLuForest="Emissions_Land_Use_Forest_Land_E_All_Data.zip",
             EmisLuGrass="Emissions_Land_Use_Grassland_E_All_Data.zip",
             EmisLuTotal="Emissions_Land_Use_Land_Use_Total_E_All_Data.zip",
             
             ValueOfProd="Value_of_Production_E_All_Data.zip",
             ForestProdTrade="Forestry_E_All_Data_(Normalized).zip")
  
  
  file <- toolSubtypeSelect(subtype,files)
  
  ## if file is .zip uncompress
  extension <- file_ext(basename(file))
  csv_name <- paste0(file_path_sans_ext(file), ".csv")
  if(extension=="zip" & !file.exists(csv_name)){
    unzip(file, exdir = tempdir())                       # use the absolute path to the file in order to unzip when working in the function
    file <- paste0(tempdir(), "/",csv_name)
    on.exit(file.remove(file))
  }
  
  ## efficient reading of csv file: read only needed columns in the needed type (codes as factor)
  csvcolnames <- colnames(read.table(file, header=T, nrows=1, sep=","))
  
  ## in case data with the years as columns has to be read in start the differentiation here
  
  if(subtype=="ForestProdTrade"){
    readcolClass <- rep("NULL",length(csvcolnames))
    readcolClass[csvcolnames=="Area.Code" | csvcolnames=="Item.Code" | csvcolnames=="Element.Code"] <- "factor"
    readcolClass[csvcolnames=="Area" | csvcolnames=="Element" | csvcolnames=="Item" | csvcolnames=="Unit"] <- "character"
    readcolClass[csvcolnames=="Value" | csvcolnames=="Year"] <- NA
    FAO <- read.table(file, header=F, skip=1, sep=",", colClasses=readcolClass, col.names=csvcolnames, quote = "\"", encoding = "latin1")
    names(FAO)[names(FAO) == "Area.Code"] <- "CountryCode"
    names(FAO)[names(FAO) == "Area"] <- "Country"
    ## list countries where no respective ISO code is available in a message
    countryandcode <- unique(FAO[,c("CountryCode","Country")])
  } else {
    readcolClass <- rep("NULL",length(csvcolnames))
    readcolClass[csvcolnames=="CountryCode" | csvcolnames=="ItemCode" | csvcolnames=="ElementCode"] <- "factor"
    readcolClass[csvcolnames=="Country" | csvcolnames=="Element" | csvcolnames=="Item" | csvcolnames=="Unit"] <- "character"
    
    if(subtype=="EmisLuTotal"){
      readcolClass[csvcolnames=="Flag" | csvcolnames=="ElementGroup"] <- NA
      readcolClass[csvcolnames=="Value" | csvcolnames=="Year"] <- NA
    } else if(subtype=="Fodder"){
      readcolClass[csvcolnames=="Value" | csvcolnames=="Year"] <- "character"
      csvcolnames <- csvcolnames[-grep("NULL", readcolClass)]
    } else {
      readcolClass[csvcolnames=="Value" | csvcolnames=="Year"] <- NA
      csvcolnames <- csvcolnames[-grep("NULL", readcolClass)]
    }
  
    FAO <- fread(input=file, header=F, skip=1, sep=",", colClasses=readcolClass, col.names= csvcolnames, quote = "\"", encoding = "Latin-1", showProgress = FALSE)
    if(all(!is.factor(FAO$CountryCode))) FAO$CountryCode <- as.factor(FAO$CountryCode)
    FAO$Value <- as.numeric(FAO$Value)
    ## list countries where no respective ISO code is available in a message
    countryandcode <- unique(FAO[,c("CountryCode","Country")])
  }
  
  ## collect the countries that do not exist in the data
  FAOiso_faocode <- toolGetMapping("FAOiso_faocode.csv", where="moinput")
  not_incl <- countryandcode$Country[!countryandcode$CountryCode %in% FAOiso_faocode$CountryCode]
  not_incl_coun <- not_incl[!grepl("(Total)",not_incl)]  
  if (length(not_incl_coun) > 0) {
    vcat(1,"The following countries were not included due to missing ISO codes:",
         "\n", paste(not_incl_coun, "\n"),"-> Consider an update of FAOiso_faocode.csv", "\n") }
  FAO <- FAO[FAO$CountryCode %in% FAOiso_faocode$CountryCode,]
  gc()
  FAO$ISO <- FAO$CountryCode
  rownames(FAOiso_faocode) <- as.character(FAOiso_faocode$CountryCode) # becomes necessary because data is now loaded as .csv
  levels(FAO$ISO) <- as.character(FAOiso_faocode[levels(FAO$CountryCode),"ISO3"])
  
  
  ### convert some units
  replace <- FAO$Unit == "1000 tonnes"
  if(any(replace)){
    FAO$Value[replace] <- FAO$Value[replace]*1000
    FAO$Unit[replace] <- "tonnes"
  }
  
  replace <- FAO$Unit == "1000 Head"
  if(any(replace)){
    FAO$Value[replace] <- FAO$Value[replace]*1000
    FAO$Unit[replace] <- "Head"
  }  
  
  replace <- FAO$Unit == "1000"
  if(any(replace)){
    FAO$Value[replace] <- FAO$Value[replace]*1000
    FAO$Unit[replace] <- "number"
  }
  
  replace <- FAO$Unit == "1000 Ha"
  if(any(replace)){
    FAO$Value[replace] <- FAO$Value[replace]*1000
    FAO$Unit[replace] <- "ha"
  }
  
  ### use ElementShort or a combination of Element and Unit instead of ElementCode
  FAOelementShort <- toolGetMapping("FAOelementShort.csv", where="moinput")
  
  elementShort <- FAOelementShort
  
  ## make ElementShort a combination of Element and Unit, replace special characters, and subsitute several _ by one
  FAO$ElementShort <- gsub("_{1,}","_", paste0(gsub("[\\.,;?\\+& \\/\\-]","_",FAO$Element, perl=TRUE),"_(",gsub("[\\.,;\\+& \\-]","_",FAO$Unit, perl=TRUE),")"), perl = TRUE)    
  ### replace ElementShort with the entries from ElementShort if the Unit is the same
  elementShort <- elementShort[elementShort$ElementCode %in% FAO$ElementCode,]
  
  if (length(elementShort) > 0) {
    for (i in 1:nrow(elementShort)) {
      FAO$ElementShort[FAO$ElementCode == elementShort[i,"ElementCode"] & FAO$Unit == elementShort[i,"Unit"]] <- as.character(elementShort[i,"ElementShort"])
    }
  }
  
  # remove accent in Mate to avoid problems
  # remove other strange names
  FAO$Item <- gsub("\u00E9","e",FAO$Item, perl=TRUE)
  FAO$Item <- gsub("\n + (Total)", " + (Total)", FAO$Item, fixed = TRUE)
  FAO$ItemCodeItem <- paste0(FAO$ItemCode,"|", gsub("\\.","",FAO$Item,perl=TRUE))    
  
  gc()
  
  FAO_mag <- as.magpie(FAO[,c("Year","ISO","ItemCodeItem","ElementShort","Value")], temporal=1, spatial=2, datacol=5)
  if(subtype == "EmisAgBurnCropResid" | subtype == "EmisAgCropResid" | subtype == "EmisLuForest") getNames(FAO_mag, dim=1) <- gsub("\\r", "", getNames(FAO_mag, dim=1))
  
  rm(FAO)
  gc()
  
  FAO_mag <- magpiesort(FAO_mag)
  
  return(FAO_mag)
}
