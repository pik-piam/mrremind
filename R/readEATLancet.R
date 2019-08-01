#' Read in data from the EAT Lancet Comission
#' 
#' Read in supplementary data for:
#' Food in the Anthropocene: the EAT-Lancet Commission on healthy diets from sustainable food systems, Lancet 2019
#' https://doi.org/10.1016/S0140-6736(18)31788-4 
#' 
#' 
#' @param subtype Type of EAT Lancet data that should be read. Available types are:
#' \itemize{ 
#' \item \code{cons_data}: Consumption analysis ("EAT_Lancet_cons_data.csv")
#' }
#' @return magpie object containing EAT Lancet Comission data
#' @author Isabelle Weindl
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="EATLancet",subtype="cons_data")
#' }
#' 
readEATLancet <- function(subtype) {
  
  if (subtype == "cons_data") { 
    data <- read.csv("EAT_Lancet_cons_data.csv",sep=",", header=TRUE,stringsAsFactors = FALSE)
    #remove the column "SSP_scn": this information is not needed because all estimates are based on SSP2
    data <- data[,!(names(data) == "SSP_scn")]
    
    #remove aggregated regions in column "region"
    data <- subset(data,!grepl("all-r|HIC|UMC|LMC|LIC",data$region))
    
    #only keep the absolute values (abs) and remove the relative measures 
    #(absolute changes relative to 2010 (chg_2010), percentage changes relative to 2010 (pct_2010))
    data <- subset(data,grepl("abs",data$measure))
    data <- data[,!(names(data) == "measure")]
    
    #re-structure the dataframe such that for all scenarios and units the respective data sets have the same size
    data <-subset(data,!grepl("total",data$food_group))
    
    #extent data frame to full dimensionality
    col.labels<- number.col.labels <- list()
    nrows.ext <-1
    for(i in 1:(dim(data)[[2]]-1)){
      col.labels[[i]] <- unique(data[[i]])
      number.col.labels[[i]] <-length(col.labels[[i]])
      nrows.ext <- nrows.ext*number.col.labels[[i]]
    }
    
    data.ext <- as.data.frame(array(NA,dim = c(nrows.ext,dim(data)[[2]])))
    names(data.ext) <- names(data)
    
    tmp.scalar <-1
    for(i in 1:(dim(data)[[2]]-1)){
      tmp.scalar <- tmp.scalar*number.col.labels[[i]]
      data.ext[[i]] <- rep(x=col.labels[[i]],each=nrows.ext/tmp.scalar,length.out=nrows.ext)
    }  
    
    data.new <-data.ext
    for(i in 1:nrows.ext){
      a<-which(data.ext[i,"kcal_scn"]==
                 data[,"kcal_scn"]&data.ext[i,"item"]==
                 data[,"item"]&data.ext[i,"diet_scn"]==
                 data[,"diet_scn"]& data.ext[i,"food_group"]==
                 data[,"food_group"]&data.ext[i,"region"]==
                 data[,"region"]&data.ext[i,"year"]==data[,"year"]
               )
      if( length(a) == 0 ){
        data.new[i,"value"] <- 0
      }
      else{
        data.new[i,"value"] <- data[a,"value"] 
      }
    }
    
    data.new$year<-paste("y",data.new$year,sep="")
   
    mdata<-as.magpie(data.new,spatial=which(colnames(data.new)=="region"),temporal=which(colnames(data.new)=="year"),datacol=dim(data.new)[2],tidy=TRUE)
    
 
  }else {
    stop("Not a valid subtype!")
  } 
  
  return(mdata)
}  
