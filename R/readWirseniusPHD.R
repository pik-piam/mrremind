#' Read Wirsenius PHD
#' 
#' Read-in Wirsenius PHD data .csv file as magclass object
#' 
#' 
#' @return magpie object of the WirseniusPHD data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="WirseniusPHD")
#' }
#' 
readWirseniusPHD <- function() {
    
      w <- read.csv("WirseniusPHD.csv", sep=";", header=TRUE)
      w$table_items_groups <- paste(w$Table, w$wirsenius_items, w$wirsenius_groups, sep=".") # merge first 3 columns
      w$wirsenius_items <- NULL
      w$wirsenius_groups <- NULL
      w$Table <- NULL
      w <- w[, c(9,1,2,3,4,5,6,7,8)] # reorder columns
      
      regions <- c("items_groups"="items_groups", "East Asia"="EAS", "East Europe"="EER", "Latin America & Carib."="LAC", "North Africa & W. Asia"="NAA", "North America & Oc."="NAO", "South & Central Asia"="SCA", "Sub-Saharan Africa"="AFR", "West Europe" ="WER")
      dimnames(w)[[2]] <- regions
      w <- as.magpie(w)
      getYears(w) <- "y2000"
      return(w)
}  
