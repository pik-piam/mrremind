#' Calculate Feed Requirements
#' 
#' Provides MAgPIE-FEED data for Feed Requirement calculated in the regression
#' for feed (calcRegressionFEED). No changes to the content have been done.
#' Usually no weight needed as the data will be used in MAgPIE-FEED model which
#' is country based.
#' 
#' 
#' @return MAgPIE-FEED data for Feed Requirement and corresonding weights as a
#' list of two MAgPIE objects
#' @author Lavinia Baumstark, Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{calcRegressionFEED}},
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FeedRequirement")
#' 
#' }
#' @importFrom luscale rename_dimnames
#' @importFrom magclass as.magpie
#' 

calcFeedRequirement <- function() {

  past<-findset("past")  

  fReq <- calcOutput("RegressionFEED",aggregate=FALSE)[,,"feed",pmatch=TRUE]
 
  output <- unwrap(fReq[,past,])
  mapping<-data.frame(
    old=c("feed_beef_gr","feed_beef_rep","feed_chicken","feed_dairy_gr","feed_dairy_rep","feed_hen","feed_pig"),
    new=c("beef_growing","beef_reproducer","chicken","dairy_growing","dairy_reproducer","hen","pig_all")
  )
  output<-rename_dimnames(output,dim = 3.1,query = mapping,from = "old", to="new")
  output <- as.magpie(output)
  
  # load weight
  # todo: use production as weight
  
      
  return(list(x=output,
              weight=NULL,
              unit="GJ per ton FM",
              description="feed energy requirements for livestock subsystems"
              ))
}
