#' @title readCEEW
#' Read Employment factors and cumulative jobs for RE techs (for India) from reports published by CEEW et al.
#' See README.txt in the source folder for more information.
#' @author Aman Malik
#' @importFrom readxl read_excel
#' @param subtype data subtype. Either "Employment factors" or "Employment"
#' @examples 
#'  \dontrun{ 
#' a <- readSource("CEEW",convert=F,subtype="Employment")
#' }


readCEEW <- function(subtype){
  if (subtype=="Employment factors")
  {
    # Using following convention for tech names
    #  techs <- c("Solar|CSP","Wind","Hydro","Biomass","Coal","Gas","Nuclear","Oil")
    #  Using following convention for activity names
    #  c("Manf","CI","OM")
    
    input <- readxl::read_excel("Employment_CEEW.xlsx",sheet = 1)
    tech <- input$Tech
    input$Tech <- gsub(x = input$Tech,pattern = "Solar \\(ground mounted\\)",replacement = "Solar|PV")
    input$Tech <- gsub(x = input$Tech,pattern = "Solar \\(rooftop\\)",replacement = "Solar|PV|Rooftop")
    
    colnames(input)[2] <- "CI"
    colnames(input)[3] <- "OM"
    
    input$CI <- input$CI*input$`Construction Period`
    input <- input[,c(1:3)]
    x <- as.magpie(input)
    #getRegions(x) <- "IND"
    
    return (x)
    
  }
  
  if(subtype=="Employment")
  {
    input <- readxl::read_excel("Employment_CEEW.xlsx",sheet = 3)
    # Assuming FY2016 to be year 2015 and FY2019 to be year 2018
    input$Year <- gsub(x = input$Year,pattern = "FY16",replacement = "2015")
    input$Year <- gsub(x = input$Year,pattern = "FY17",replacement = "2016")
    input$Year <- gsub(x = input$Year,pattern = "FY18",replacement = "2017")
    input$Year <- gsub(x = input$Year,pattern = "FY19",replacement = "2018")
    input$Tech <- gsub(x = input$Tech,pattern = "Utility-scale Solar",replacement = "Solar|PV")
    input$Tech <- gsub(x = input$Tech,pattern = "Rooftop Solar",replacement = "Solar|PV|Rooftop")
    
    x <- as.magpie(input,spatial=NULL)
    # getRegions(x) <- "IND"
    
    return (x)
    
  }
}