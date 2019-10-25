#' @title calcTimberDemand
#' @description 
#' Calculates the demand of timber from FAO data (including intermediate products).
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{calcFAOmassbalance_pre}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("TimberDemand")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @export

calcTimberDemand <- function(){
  x <- readSource("FAO","ForestProdTrade")
  
  ## Remove distinction between coniferous and non-coniferous part
  x <- x[,,sort(getNames(x)[grep(pattern = "coniferous",x = getNames(x),invert = T)])]
  
  ## Remove monetary data, we are only interested in m3 or mio. ton data
  x <- x[,,sort(getNames(x)[grep(pattern = "Value",x = getNames(x),invert = T)])]
  
  ## Extract variables we need and only choose variables which which have data for all three categories:
  ## Production in m3
  ## Import in m3
  ## Export in m3
  ## Here, wood pulp is an exception which is in mio. tonnes so we will assume 450 kg/m3 density there
  
  variables_needed <- c("Roundwood",
                        "Industrial roundwood","Wood fuel",
                        "Other industrial roundwood","Pulpwood, round and split, all species (production)",
                        "Sawlogs and veneer logs","Fibreboard","Particle board and OSB",
                        "Wood pulp","Sawnwood","Plywood","Veneer sheets","Wood-based panels")
  ## in above variables, remeber that Sawlogs and veneer logs need an additional category called "SLVL based wood" which we will calculate back based on the sum of Sawnwood, plywood, venner sheets, wood residues and wood chips. 
  ## Also remeber that Pulpwood round and split only has data for production and not import and export so that has to be calculated back based on its constituents. 
  ## See https://i.imgur.com/VackFiv.png for better clarity.
  
  timber_fao <- x[,,variables_needed] ### Remember that wood pulp is in mio tonnes so we have to convert it to m3
  
  ## We'll consider the production, import and export separately
  
  production <- timber_fao[,,grep(pattern = "Production_",x = getNames(timber_fao))]
  import <- timber_fao[,,grep(pattern = "Import_Quantity_",x = getNames(timber_fao))]
  export <- timber_fao[,,grep(pattern = "Export_Quantity_",x = getNames(timber_fao))]
  
  ## First, lets give correct names in the dimensions i.e. get rid of underscores
  getNames(production) <- gsub(x = getNames(production),pattern = "Production_",replacement = "production ")
  getNames(production) <- gsub(x = getNames(production),pattern = "Mio_tonnes",replacement = "mio t")
  ## Now we get rid of big name string of pulpwood
  getNames(production) <- gsub(x = getNames(production),pattern = "Pulpwood, round and split, all species \\(production)",replacement = "Pulpwood")
  
  getNames(import) <- gsub(x = getNames(import),pattern = "Import_Quantity_",replacement = "import ")
  getNames(import) <- gsub(x = getNames(import),pattern = "Mio_tonnes",replacement = "mio t")
  
  getNames(export) <- gsub(x = getNames(export),pattern = "Export_Quantity_",replacement = "export ")
  getNames(export) <- gsub(x = getNames(export),pattern = "Mio_tonnes",replacement = "mio t")

  ## Next step is to convert wood pulp from mio t to m3
  
  production[,,"Wood pulp"] <- production[,,"Wood pulp"] * 1000000 * 1000 / 450  ## 10^6 * 10^3 for mio t to kg. 450 for kg to m3
  import[,,"Wood pulp"] <- import[,,"Wood pulp"] * 1000000 * 1000 / 450  ## 10^6 * 10^3 for mio t to kg. 450 for kg to m3
  export[,,"Wood pulp"] <- export[,,"Wood pulp"] * 1000000 * 1000 / 450  ## 10^6 * 10^3 for mio t to kg. 450 for kg to m3
  
  ## Now we get rid of mio t in units for wood pulp
  getNames(production)  <- gsub(x = getNames(production),pattern = "mio t",replacement = "m3")
  getNames(import)      <- gsub(x = getNames(import),pattern = "mio t",replacement = "m3")
  getNames(export)      <- gsub(x = getNames(export),pattern = "mio t",replacement = "m3")
  
  ## in the import and export objects, we need to add segment for pulpwood which is the sum of fibreboard, particlea board OSB and Wood pulp
  
  to_add <- setdiff(getNames(production,dim = 1),getNames(export,dim = 1))
  
  import <- add_columns(x = import,addnm = to_add,dim = 3.1)
  export <- add_columns(x = export,addnm = to_add,dim = 3.1)
  
  import[,,to_add] <- 0 ## No import and export data for pulpwood from FAO
  export[,,to_add] <- 0 ## No import and export data for pulpwood from FAO
  
  ## Another category to add here is "Other Sawnwood" (refer to )
  to_add <- "Other sawnwood"
  production <- add_columns(x = production,addnm = to_add,dim = 3.1)
  production[,,to_add] <- production[,,"Sawlogs and veneer logs"] - dimSums(production[,,c("Plywood","Veneer sheets","Sawnwood")],dim = 3)
  production[,,to_add][production[,,to_add]<0] = 0
  import <- add_columns(x = import,addnm = to_add,dim = 3.1)
  export <- add_columns(x = export,addnm = to_add,dim = 3.1)
  import[,,to_add] <- 0 ## No import and export data for pulpwood from FAO
  export[,,to_add] <- 0 ## No import and export data for pulpwood from FAO
  
  ## Now we are ready to merge back production, import and export
  timber_fao_cleaned <- mbind(production,import,export)
  
  ###### Correction Stage
  ###### Refer to http://dx.doi.org/10.1016/j.rser.2016.09.107
  ###### Paper titled "The wood from the trees: The use of timber in construction"
  
 
  ###### Data cleaning stage
  timber_fao_cleaned <- add_columns(timber_fao_cleaned,addnm = "other_util",dim = 3.2)
  timber_fao_cleaned[,,"other_util"] <- timber_fao_cleaned[,,"production (m3)"] + timber_fao_cleaned[,,"import (m3)"] - timber_fao_cleaned[,,"export (m3)"]
  
  timber_fao_cleaned <- add_columns(timber_fao_cleaned,addnm = "domestic_supply",dim = 3.2)
  timber_fao_cleaned[,,"domestic_supply"] <- timber_fao_cleaned[,,"other_util"]
  
  ## Now convert data to mio. m3
  timber_fao_cleaned <- timber_fao_cleaned / 1000000
  getNames(timber_fao_cleaned) <- gsub(x = getNames(timber_fao_cleaned), pattern = "m3", replacement = "mio m3")
  getNames(timber_fao_cleaned) <- gsub(x = getNames(timber_fao_cleaned), pattern = "domestic_supply", replacement = "domestic_supply (mio m3)")
  getNames(timber_fao_cleaned) <- gsub(x = getNames(timber_fao_cleaned), pattern = "other_util", replacement = "other_util (mio m3)")
  
  timber_fao_cleaned[timber_fao_cleaned<0] = 0
  

  
  pop <- setNames(readSource("WDI",subtype="SP.POP.TOTL",convert = T),"weight")
  pop <- pop[,intersect(getYears(timber_fao_cleaned),getYears(pop)),]
  
  timber_fao_cleaned <- timber_fao_cleaned[,intersect(getYears(timber_fao_cleaned),getYears(pop)),]
  
  getNames(timber_fao_cleaned) <- gsub(x = getNames(timber_fao_cleaned), pattern = " \\(mio m3)", replacement = "")
  
  # weight <- y
  # weight[,,] <- 1
  
  out <- timber_fao_cleaned
  
  # imp_countries <- where(out[,,"Roundwood"]>1)$true$regions
  # 
  # out <- out[imp_countries,,]
  
  return(list(x=out,
              weight=NULL,
              min=0,
              unit="mio m3",
              description="Calculates the timber demand pattern based on historical FAO data"))
  
  }
  