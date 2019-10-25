#' @title calcWBGEM
#' @description  recover world price of commidity in terms of real 2005 USD per metric ton
#' @return  magpie object of time series of world price of commodites
#' @author Xiaoxi Wang
#' @seealso 
#' \code{\link{readSource}},
#' \code{\link{readWBGEM}}
#' 
#' @examples 
#' 
#' \dontrun{
#' calcOutput("WBGEM")}
#' 
#' @importFrom magclass mbind


calcWBGEM <- function(){
  x <- readSource("WBGEM")
  sugar <- grep("world",grep("Sugar",getNames(x),value=TRUE),value=TRUE)
  adjustFactor2010 <- setNames(x[,,grep("real",sugar,value=TRUE)]/setNames(x[,,grep("nominal",sugar,value=TRUE)],NULL),NULL)
  adjustFactor2005 <- 1/setYears(x[,2005,grep("real",sugar,value=TRUE)]/setNames(x[,2005,grep("nominal",sugar,value=TRUE)],NULL),NULL) * adjustFactor2010
  adjustFactor2005 <- setNames(adjustFactor2005,NULL)
  
  vars <- grep("nominal",getNames(x),value=T)
  tmp1 <- x[,,grep("[/]kg",vars,value=TRUE)]*1000
  tmp2 <- x[,,grep("[/]mt",vars,value=TRUE)]
  tmp3 <- x[,,grep("Agr",vars,value=TRUE)]
  
  .rename <- function(x){
    var_name <- getNames(x)
    var_name <- gsub("_nominal","",gsub("\\$","",gsub("[{^\\#&~_/<>'!,:.;`\"}@-]","_",gsub("[ /\t\n\r\f\v]","",var_name))))
    var_name <- gsub("_kg","",var_name)
    var_name <- gsub("_mt","",var_name)
    getNames(x) <- var_name
    return(x)
  }
  
  out <- mbind(.rename(tmp1),.rename(tmp2),.rename(tmp3))
  out <- out* adjustFactor2005
  out[is.na(out)] <- 0
  return(list(x=out,
              unit= "real2005 USD per ton",
              weight= NULL,
              description = "WBGEM global price of commodity"))
}
