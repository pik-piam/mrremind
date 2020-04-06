#' toolNUTSAggregate
#' 
#' Converts gridded magpie data to EC NUTS 2016 level, from here https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
#' @param input magpie object on cellular level
#' @param nuts_lvl 1, 2, or 3 for the NUTS aggregation level, 3 most disaggregated
#' @param relative If unit is relative (i.e. yields etc) set to TRUE for averaged result
#' @return return array on NUTS level
#' @author David Chen, Benjamin Leon Bodirsky
#' 
#' @importFrom utils read.csv2
#' @importFrom magclass as.magpie
#' @export

toolNUTSaggregate <- function(input, nuts_lvl, relative){

##read in matrix mapping of NUTS to magpie grid cells
path <- toolMappingFile("cell", name=paste0("nuts",nuts_lvl,".csv"))
cm <- read.csv2(path, header=T, row.names = 1)
cm <- as.matrix(cm)

#read in input
if((length(input)==1)){
  if(is.character(input)){x <- read.magpie(input)
  }else {stop("unknown format of input")}
} else if(!is.magpie(input)){
  stop("unknown format of input")
} else {x=input}

#make sure input cells matches mapping, country cell level
x <- toolCell2isoCell(x)

##subset to european grid cells
cmr <- as.magpie(cm, spatial=1)
x <- x[(dimnames(cm)[[1]]),,]

# input as array
arr <- as.array(x)

# create output array
C <- array(NA, dim=c(length(x[1,1,]), ncol(cm), ncol(arr)))

# fill output array by matrix multiplication
for (i in 1:length(arr[1,1,])){
  for (j in 1:ncol(arr))
  C[i,,j] <- arr[,j,i] %*% cm
}

## if relative unit, divide by total sum of cell values ##

if (relative==TRUE){
  rel <- rep(0,times=ncol(cm))
  names(rel) <- colnames(cm)

  for(i in 1:ncol(cm)) {
     rel[i] <- sum(cm[which(cm[,i]>0),i])
      }
  C <-sweep(C, 2, rel, "/")
}

#write dimnames
C <-aperm(C, perm=c(2,3,1))
dimnames(C) <- c(dimnames(cm)[2], dimnames(arr)[2], dimnames(arr)[3])
C <- as.magpie(C,spatial=1)
return(C)
}

