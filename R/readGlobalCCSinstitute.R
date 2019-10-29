#' @importFrom readxl read_excel


readGlobalCCSinstitute<- function() {
  
   x <- read_excel("status-ccs-project-database-current-08-09-2017.xlsx",skip=3)
   # delete not needed information
   x$`Facility name (click on link to view)` <- NULL
   x$`Lifecycle stage`                       <- NULL
   #x$Industry                                <- NULL
   x$`Capture type`                          <- NULL
   x$`Transport type`                        <- NULL
   x$`Transport length (km)`                 <- NULL
   x$`Primary storage type`                  <- NULL
   # transfer CO2 capture rate into one numeric number
   x$value <- gsub("Approx. ","",x$`CO2 capture capacity (Mtpa)`)
   x$value <- gsub("[0-9].[0-9]-","",x$value)
   x$`CO2 capture capacity (Mtpa)` <- NULL
   # transfer x$value into numeric
   x$value <- as.numeric(x$value)
   # convert into a magpie object
   x <- as.magpie(x,spatial=1,datacol=5)
   x[is.na(x)] <- 0
  return(x)
}

