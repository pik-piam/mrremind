#' Read in ImageMacc Costcurves for different subtypes
#' 
#' Read in ImageMacc Costcurves for different subtypes. Rows are removed, the
#' dataframe is reshaped and numbers are replaced by descriptions.
#' 
#' 
#' @param subtype data subtype. Either "CH4_Energy_Industry", "CH4_Landuse",
#' "N2O_Energy_Industry", "N2O_Landuse", "HFC_tot", "SF6_tot", "PFC_tot" or
#' "baseline_sources"
#' @return magpie object of the ImageMacc data
#' @author Nele Steinmetz
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- readSource("ImageMacc","CH4_Energy_Industry")
#' a <- readSource("ImageMacc","CH4_Landuse")
#' a <- readSource("ImageMacc","N2O_Energy_Industry")
#' a <- readSource("ImageMacc","N2O_Landuse")
#' a <- readSource("ImageMacc","HFC_tot")
#' a <- readSource("ImageMacc","SF6_tot")
#' a <- readSource("ImageMacc","PFC_tot")
#' a <- readSource("ImageMacc","baseline_sources")
#' 
#' }
#' @importFrom stats reshape
readImageMacc <- function(subtype) {
  
  files <- c(CH4_Energy_Industry="Costcurves_CH4_Energy_Industry.csv",
              CH4_Landuse="Costcurves_CH4_landuse.csv",
               N2O_Energy_Industry="Costcurves_N2O_Energy_Industry.csv",
               N2O_Landuse="Costcurves_N2O_landuse.csv",
               HFC_tot="Costcurves_HFCtot.csv",
               SF6_tot="Costcurves_SF6tot.csv",
               PFC_tot="Costcurves_PFCtot.csv",
               baseline_sources="Costcurves_baseline_sources.csv")
  
  file <- toolSubtypeSelect(subtype,files)
  
  data <- read.csv(file, sep=";", stringsAsFactors=FALSE)
  
  if(subtype=="CH4_Energy_Industry"){
    # get first rows
    head <- data[2:4,1]
    # remove rows of dataframe
    data <- data[-c(1:6),]
    # define first row as header
    names(data) <- data[1,]
    data <- data[-1,]
    # tidy dataframe
    data <- reshape(data, idvar=c("t2", "Source", "Region"), 
                     timevar="class", v.names="value", varying=list(4:ncol(data)), 
                     direction="long")
    data$value <- as.numeric(as.character(data$value))
     # replace numbers in Source
    head <- strsplit(head, ") ")
    head <- unlist(head)
    data$Source[data$Source==1] <- "CH4 coal losses/leakages"
    data$Source[data$Source==2] <- "CH4 oil losses/leakages"
    data$Source[data$Source==3] <- "CH4 natural gas losses/leakages"
    data <- data[,c(1,3,2,4,5)]
    }
  
  if(subtype=="CH4_Landuse") {
    head <- data[2:6,1]
    data <- data[-c(1:8),]
    names(data) <- data[1,]
    data <- data[-1,]
    data <- reshape(data, idvar=c("t2", "Source", "Region"), 
                    timevar="class", v.names="value", varying=list(4:ncol(data)), 
                    direction="long")
    data$value <- as.numeric(as.character(data$value))
    head <- strsplit(head, ") ")
    head <- unlist(head)
    data$Source[data$Source==1] <- "CH4 Landfills"
    data$Source[data$Source==2] <- "CH4 Domestic Sewage"
    data$Source[data$Source==3] <- "CH4 Wetland rice"
    data$Source[data$Source==4] <- "CH4 Animals"
    data$Source[data$Source==5] <- "CH4 Animal waste"
    data <- data[,c(1,3,2,4,5)]
    }
   
  if(subtype=="N2O_Energy_Industry"){
    head <- data[2:4,1]
    data <- data[-c(1:6),]
    names(data) <- data[1,]
    data <- data[-1,]
    data <- reshape(data, idvar=c("t2", "Source", "Region"), 
                    timevar="class", v.names="value", varying=list(4:ncol(data)), 
                    direction="long")
    data$value <- as.numeric(as.character(data$value))
    head <- strsplit(head, ") ")
    head <- unlist(head)
    data$Source[data$Source==1] <- "N2O Transport"
    data$Source[data$Source==2] <- "N2O Adipic acid production"
    data$Source[data$Source==3] <- "N2O Nitric acid production"
    data <- data[,c(1,3,2,4,5)]
    }

  if(subtype=="N2O_Landuse"){
    head <- data[2:4,6]
    data <- data[-c(1:6),]
    names(data) <- data[1,]
    data <- data[-1,]
    data <- reshape(data, idvar=c("t2", "Source", "Region"), 
                    timevar="class", v.names="value", varying=list(4:ncol(data)), 
                    direction="long")
    data$value <- as.numeric(as.character(data$value))
    head <- strsplit(head, ") ")
    head <- unlist(head)
    data$Source[data$Source==1] <- "N2O Fertilizer"
    data$Source[data$Source==2] <- "N2O Animal waste"
    data$Source[data$Source==3] <- "N2O Domestic sewage"
    data <- data[,c(1,3,2,4,5)]
    }
  
  if(subtype=="HFC_tot"){
    data <- data[-c(1:4),]
    names(data) <- data[1,]
    data <- data[-1,]
    data <- reshape(data, idvar=c("t2", "Region"), 
                    timevar="class", v.names="value", varying=list(3:ncol(data)), 
                    direction="long")
    data$value <- as.numeric(as.character(data$value))
    }
  
  if(subtype=="SF6_tot"){
    data <- data[-c(1:4),]
    names(data) <- data[1,]
    data <- data[-1,]
    data <- reshape(data, idvar=c("t2", "Region"), 
                    timevar="class", v.names="value", varying=list(3:ncol(data)), 
                    direction="long")
    data$value <- as.numeric(as.character(data$value))
    }
  
  if(subtype=="PFC_tot"){
    data <- data[-c(1:4),]
    names(data) <- data[1,]
    data <- data[-1,]
    data <- reshape(data, idvar=c("t2", "Region"), 
                    timevar="class", v.names="value", varying=list(3:ncol(data)), 
                    direction="long")
    data$value <- as.numeric(as.character(data$value))
   }
  
  else if(subtype=="baseline_sources"){
    names(data) <- data[1,]
    data <- data[-1,]
    data <- data[-c(5:11)]
    data$Emission <- as.numeric(as.character(data$Emission))
    data <- reshape(data, idvar=c("time", "SRC", "Region"), 
                    timevar="Emission", varying=list(4:ncol(data)), 
                    direction="long")
    data$SRC[data$SRC==6] <- "CH4 coal losses/leakages"
    data$SRC[data$SRC==7] <- "CH4 oil losses/leakages"
    data$SRC[data$SRC==8] <- "CH4 natural gas losses/leakages"
    data$SRC[data$SRC==9] <- "CH4 Landfills"
    data$SRC[data$SRC==10] <- "CH4 Domestic Sewage"
    data$SRC[data$SRC==11] <- "CH4 Wetland rice"
    data$SRC[data$SRC==12] <- "CH4 Animals"
    data$SRC[data$SRC==13] <- "CH4 Animal waste"
    data$SRC[data$SRC==14] <- "N2O Transport"
    data$SRC[data$SRC==15] <- "N2O Adipic acid production"
    data$SRC[data$SRC==16] <- "N2O Nitric acid production"
    data$SRC[data$SRC==17] <- "N2O Fertilizer"
    data$SRC[data$SRC==18] <- "N2O Animal waste"
    data$SRC[data$SRC==19] <- "N2O Domestic sewage"
    data$SRC[data$SRC==20] <- "HFC"
    data$SRC[data$SRC==21] <- "PFC"
    data$SRC[data$SRC==22] <- "SF6"
    data <- data[data$SRC!="23",]
    data <- data[,c(1,3,2,4)]
  }

  x<- as.magpie(data, temporal=1, spatial=2, tidy=TRUE)
  regions <- c("1"="CAN","2"="USA","3"="CAM","4"="SAM","5" ="NAF",
             "6"="WAF","7"="EAF","8"="SAF", "9"="WEU", "10"="CEU",
             "11"="FSU", "12"="MID", "13"="SAS", "14"="EAS", "15"="SEA",
             "16"="OCE", "17"="JAP")
  
  row.names(x) <- regions
  # dimnames(test)[[1]] <- regions

return(x)
}
