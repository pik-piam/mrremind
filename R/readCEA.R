#' @title readCEA
#' Read Employment factors and cumulative jobs for RE techs from reports published in CEA's NEP
#' See README.txt in the source folder for more information.
#' @author Aman Malik
#' @importFrom readxl read_excel
#' @examples 
#'  \dontrun{ 
#' a <- readSource("CEA",convert=F)
#' }
#' @seealso \code{\link{readSource}}
#' @importFrom magclass add_dimension
#' @return magpie object containing employment factors and cumulative jobs for solar PV, solar rooftop, and wind

readCEA <- function()
{
  # Convention of technology names
 # techs <- c("Solar|PV","Solar|CSP","Wind","Hydro","Biomass","Coal","Gas","Nuclear","Oil")
  
  # employment intensity assumed to be only for O&M
  input <- readxl::read_excel("Employment_factors_CEA.xlsx")
  colnames(input)[1] <- "Tech"
  input <- input[,c(1,4)] # removing intensity values for "technical" and "non-technical" parts and 
  # considering only "total" values.
  input$Tech <- gsub(x = input$Tech,pattern = "Thermal",replacement = "Coal")
  input$Tech <- gsub(x = input$Tech,pattern = "Hydro",replacement = "Hydro")
  input$Tech <- gsub(x = input$Tech,pattern = "Solar",replacement = "Solar|PV")
  input$Region <- "IND"
  x <- as.magpie(input)
  x <- add_dimension(x,dim = 3.2,add = "activity",nm = "OM")# # employment intensity assumed to be only for O&M

  return (x)
}
