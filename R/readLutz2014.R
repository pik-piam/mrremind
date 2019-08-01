#' @title readLutz2014
#' @description It reads and clears the dataset of the global population projections 
#' by age, sex and education, available on the Wittgenstein Centre Data Explorer
#' and published by Lutz, Butz and K. C. , 2014. "Population and human capital 
#' in the twenty-first century" Oxford University Press. 
#' From .csv file to a magclass object
#' 
#' @return magpie object with the dataset downloaded. It contains missing values
#' and it is possible to replace them with the function convertLutz2014.
#' 
#' @seealso
#' \code{\link{convertLutz2014}}
#' 
#' @importFrom reshape2 acast
#' @importFrom countrycode countrycode
#' @importFrom utils read.table

readLutz2014 <- function() 
{
  merge=NULL
  for(i in (1:5))
  {
    scenario=paste0("SSP",i)
    for (gender in c("Both","M-F"))
    {
      filename=paste0("wicdf ",scenario," ",gender,".csv")
      d <- read.table(file=filename, skip=8, 
                      quote="\"",header=TRUE , sep=",")
      #d <- d[,-1]
      if(length(d) == 5) { #"Both"
        Sex <- rep(gender 
                   , times=length(d[,1]))
        target <- which(names(d) == "Age")
        d <- cbind(d[,1:target,drop=F], Sex, 
                   d[,(target+1):length(d),drop=F])
      } 
      
      #change country codes
      d[,1] <- countrycode(d[,1],"country.name","iso3c")
      
      #remove NAs
      d<-d[-which(is.na(d[,1])),]
      
      #add "y"  in front of each the years
      d[,2] <- paste(rep("y",times=length(d[,2])), d[,2], sep="", collapse=NULL)
      
      #tranform into magpie object
      out <- acast(d, Area~Year~Sex~Age~Education,
                   value.var=names(d)[6])
      out <-as.magpie(out)
      out <- add_dimension(out,dim=3.1,add="Scenario",nm=scenario)
      #assign(paste0(gender,"_",scenario),out)
      merge <- mbind(merge,out)  
    }
    #merge <- mbind(merge,out)
  }
  getSets(merge)<-c("country","year","scenario","sex","age","education")
  merge=merge/1000
  return(merge)
}
