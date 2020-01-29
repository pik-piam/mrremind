#' Employment factors for various power production technologies
#' @author Aman Malik
#' @importFrom readxl read_excel    


readEmploymentfactor <- function(){
  
  # read in Employment factors for different technologies
  
  input <- read_excel("Jobs_empfactor.xlsx",col_names = T)
  x <- as.magpie(input)
  return (x)

}
