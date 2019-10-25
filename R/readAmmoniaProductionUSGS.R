#' readAmmoniaProductionUSGS
#' @aliases readAmmoniaProductionUSGS convertAmmoniaProductionUSGS
#' @description 
#' Function to read the the global Ammonia Production from USGS Website
#' \url{https://minerals.usgs.gov/minerals/pubs/commodity/nitrogen/}
#'
#' @return A MAgPIE-Object containing global Ammonia production from the USGS
#'
#' @examples
#' \dontrun{
#' a <- readSouce("AmmoniaProductionUSGS")
#' }
#' @importFrom reshape2 melt
readAmmoniaProductionUSGS <- function(){
  files <- c(paste0("nitrogen_", c("1998_2002", "2000_2004", "2005_2009", "2010_2014"), "_USGS.csv"))
  f0 <- function(x){
    return(length(readLines(x)))
  }
  nrows_files <- sapply(files, FUN=f0)
  add_rows <- c(7,8,9,10)
 # data_list <- lapply(X = files, FUN = read.csv, skip=5,stringsAsFactors = FALSE, dec="," )
  data_list <- mapply(FUN=read.csv, file=files, nrows=nrows_files-add_rows-5, 
                      MoreArgs = list(skip=5, stringsAsFactors=FALSE, dec=",", na.strings="--"))#, 
                    # colClasses=c("character", NA, "integer", NA, "integer", NA, "integer", NA, "integer", NA, "integer")))
  f1 <- function(i, data){
    return(data[[i]][,c(1,3,5,7,9,11)])
  }
  data_list2 <- lapply(c(1:4), f1, data=data_list)
  f2 <- function(col, data){
    oldwarn <- getOption("warn")
    options(warn=-1)
    on.exit(options(warn=oldwarn))
    return(as.integer(data[,col]))
  }
  missspelled <- c("Afghanistane", "Albaniae", "Austriae", "Bosnia and Herzegovinae",
                  "Cubae", "Denmarke", "Finlande", "Francee",  "Iraqe",  "Korea, Northe", 
                  "Netherlandse", "Nigeriae","Perue", "Tajikistane", "Turkmenistane", "Zimbabwee")
  f3 <- function(i ,col, data, missspelled){
    out <- data
    out<- lapply(col, FUN=f2, data=data[[i]])
   data[[i]][,1] <- gsub("\\d", "", data[[i]][,1])
   data[[i]][,1] <- gsub("(?<!\\w)\\s{2,}(?=\\w)", "", data[[i]][,1], perl = TRUE)
   data[[i]][,1] <- gsub("(?<=\\w)\\s{1,}(?!\\w)", "", data[[i]][,1], perl = TRUE)
   data[[i]][,1] <- gsub("(?<=\\w)([,]\\s)(?=$)", "", data[[i]][,1], perl = TRUE)
   wrongnames <- match(missspelled, data[[i]][,1], nomatch = FALSE)
   for(j in wrongnames){
     data[[i]][j,1] <- substr(x = data[[i]][j,1], start = 1, stop = nchar(data[[i]][j,1])-1)
   }
   
    out <- as.data.frame(cbind(as.character(data[[i]][,1]), 
                               as.data.frame(out, stringsAsFactors=FALSE)),
                               stringsAsFactors=FALSE)
    colnames(out) <- colnames(data[[i]])
    out <- melt(out)
    out <- as.magpie(out,datacol=3, spatial=1, temporal=2)
    getYears(out) <- gsub("[X](?=\\d)", "y", getYears(out), perl=TRUE)
    return(out)
   # return(lapply(col, FUN=f2, data=data[[i]]))
  }
  
 
  
   data_list3 <- lapply(X = c(1:4), FUN = f3,col=c(2:6), data=data_list2, missspelled=missspelled)
   tmp <- data_list3[[1]]
   data_list3[[1]] <- tmp[,c(1998,1999),]
   f4 <- function(i, data){
     return(as.character(getRegions(data[[i]])))
   }
   countries <- lapply(X = c(1:4), FUN = f4, data=data_list3) 
  tmp <- sort(unique(Reduce(append, countries)))
  
  out <- new.magpie(cells_and_regions = tmp, years = c(1998:2014),names = "Nitrogen Production Mt N", fill = NA, sets = c("Country", "Year","Data"))
  
  out[getRegions(data_list3[[1]]),getYears(data_list3[[1]]),] <- data_list3[[1]]
  out[getRegions(data_list3[[2]]),getYears(data_list3[[2]]),] <- data_list3[[2]]
  out[getRegions(data_list3[[3]]),getYears(data_list3[[3]]),] <- data_list3[[3]]
  out[getRegions(data_list3[[4]]),getYears(data_list3[[4]]),] <- data_list3[[4]]
  
  out=out/1000
  return(out)
  
  # 
  # file <- "ds140-nitro.csv"
  # data <- read.csv(file, skip=7, stringsAsFactors = FALSE, dec=",")
  # data <- as.data.frame(cbind(data$Year, data$World.production))
  # colnames(data) <- c("Year","World_Production")
  # out <- as.magpie(data, datacol=2)
  # getNames(out) <-  "Ammonia Production in t"
  # return(out)
}
