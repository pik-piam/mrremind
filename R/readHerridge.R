#' @title readHerridge
#' @description Reads a dataset containing values for biological nitrogen fixation in agricultural systems. Source: Herridge D. F., Peoples M. B., Boddey R. M.: Global inputs of biological nitrogen fixation in agricultural systems
#' @details Availables Subtypes:
#'  \itemize{
#'  \item ndfa:  National values for Plant associated fixation
#'  \item freeliving: Global values for free living agents
#'  }
#' @param subtype a subtype for the calculation
#' @return A MAgPIE object containting the share of Nr derived from
#' \itemize{
#' \item ndfa: fixation for each country and each commodity.
#' \item freeliving: fixation by free living agents
#' }
#' @author Stephen Wirth
#' @examples
#' 
#'   \dontrun{
#'     x <- readSource("Herridge", "ndfa")
#'     x <-  readSource("Herridge", "freeliving", convert=F)
#'   }
#' 
readHerridge <- function(subtype=NULL){
  
  
  
  #file to read
  files <- c(ndfa="ndfa.csv",
             freeliving ="freeliving.csv")
  
  file <- toolSubtypeSelect(subtype,files)
  #read file
  if (subtype=="ndfa"){
    data <- read.csv(file = file, header = T,stringsAsFactors = F, skip=4)
    dimnames <- data[,1]
    data[,1] <- NULL
    #reformat file
    data <- t(data)
    #dimnames <- data[1,]
    #data <- data[-1,]
    dimnames(data)[[2]] <- dimnames
    data <- as.data.frame(data)
    #create magpie obect containing data from file
    d <- as.magpie(data)
    # set the year for the data TODO: Confirm year
    getYears(d) <- "y2005"
    #Name Sets
    getSets(d) <- c("region", "year", "groups")
  } else if (subtype=="freeliving") {
    data <- read.csv(file=file, header=F, stringsAsFactors = F)
    d <- new.magpie(years = "y2005", names = data[,1], sets = c("region", "year", "data"))
    d[,,] <- data[,2]
  }
  #return magpieobject
  return(d)
   
}