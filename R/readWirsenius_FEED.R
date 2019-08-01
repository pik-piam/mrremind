#' Read Wirsenius_FEED
#' 
#' Read-in an Wirsenius_FEED data .csv files as one magclass object
#' 
#' 
#' @return magpie object of the Wirsenius_FEED data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("Wirsenius_FEED")
#' }
#' @importFrom reshape2 melt
readWirsenius_FEED <- function(){

  files <-  c(feed_beef_gr="feed_regression_beef_growing.csv",
              feed_beef_rep="feed_regression_beef_reproducer.csv",
              feed_chicken="feed_regression_chicken.csv",
              feed_dairy_gr="feed_regression_dairy_growing.csv",
              feed_dairy_rep="feed_regression_dairy_reproducer.csv",
              feed_hen="feed_regression_hen.csv",
              feed_pig="feed_regression_pig_all.csv",
              nutrient_beef_gr="nutrient_regression_beef_growing.csv",
              nutrient_beef_rep="nutrient_regression_beef_reproducer.csv",
              nutrient_dairy_gr="nutrient_regression_dairy_growing.csv",
              nutrient_dairy_rep="nutrient_regression_dairy_reproducer.csv")
  
  # read in the data from all files
  data <- list()
  for (i in 1:length(files)){
    # read data
    x <- read.csv(files[i])
    # add col with meat type
    x$dummy <- paste(names(files[i]),x$dummy,sep=".")
    # melt data to have them in the long format
    x <- suppressMessages(melt(x,id.vars="dummy"))
    # transfer into a magpie-object
    data[[i]] <- as.magpie(x,datacol=3)
  }
  # combine all data into one magpie-object
  output <- mbind(data)
    
  return(output)
}

