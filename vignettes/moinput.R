## ----setup, include = FALSE----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",cache = T,
  eval = FALSE
)

## ---- echo=FALSE,message=FALSE,eval=FALSE--------------------------------------------------------------------------------------
#  # load moinput package
#  library(moinput)
#  library(magclass)

## ----eval=FALSE----------------------------------------------------------------------------------------------------------------
#  #' Read IRENA
#  #' Read-in an IRENA csv file as magclass object
#  #' @param subtype data subtype. Either "Capacity" or "Generation"
#  #' @return magpie object of the IRENA data with historical electricity renewable capacities (MW) or generation levels (GWh)
#  #' @author Renato Rodrigues
#  #' @seealso \code{\link{readSource}}
#  #' @examples
#  #' \dontrun{ a <- readSource(type="IRENA",subtype="Capacity")
#  #' }
#  #' @importFrom reshape2 melt
#  
#   readIRENA <- function(subtype) {
#     if (subtype == "Capacity") {
#       #Reading renewables electricity capacity values in MW from csv
#       data <- read.csv("Capacity.csv",sep=";")
#     } else if (subtype== "Generation") {
#       #Reading renewables electricity generation values in GWh from csv
#       data <- read.csv("Generation.csv",sep=";")
#     }else {
#       stop("Not a valid subtype!")
#     }
#     # data in wide format
#     data <- melt(data, id.vars=c("Country.area", "Technology"), variable.name="years", value.name="value")  # melt requires library(reshape2)
#     #replacing X by y on years preffix
#     data$years <- gsub("X", "y", data$years)
#     # rearrange column order to more readable format: year, country, tech, value (capacity or generation)
#     data <- data[,c(3,1,2,4)]
#     # creating capacity or generation magpie object
#     x <- as.magpie(data,temporal=1,spatial=2,datacol=4)
#     return(x)
#   }

## ----readsource, results='hide',message=FALSE----------------------------------------------------------------------------------
#  x <- readSource("IRENA",subtype = "Capacity",convert = FALSE)

## ----eval=FALSE----------------------------------------------------------------------------------------------------------------
#  x <- readSource("IRENA",subtype = "Capacity")

## ------------------------------------------------------------------------------------------------------------------------------
#  madrat::toolCountry2isocode(c("Japan","Australia","Afghanistan"))

## ------------------------------------------------------------------------------------------------------------------------------
#  madrat::toolCountry2isocode(c("Shire","Mordor"),mapping = c("Shire"="SHR","Mordor"="MOR"))

## ----message=FALSE-------------------------------------------------------------------------------------------------------------
#  new.magpie("GLO",years = NULL, fill= NA) # creates a region with no data on the variable "years"" and with NA as data
#  c <- new.magpie(c("IND","JPN"),years = c(1990,2000))

## ----eval=FALSE----------------------------------------------------------------------------------------------------------------
#  # If x is an existing magpie-object.
#  y <- new.magpie(getRegions(x),getYears(x),fill=NA)
#  

## ----eval=FALSE----------------------------------------------------------------------------------------------------------------
#  View(as.data.frame(x))

## ----include=FALSE-------------------------------------------------------------------------------------------------------------
#  x <- readSource("IRENA",subtype = "Capacity")
#  head(getRegions(x[,"y2010","Solar photovoltaic"]>1000))
#  # OR
#  where(x[,"y2010","Solar photovoltaic"]>1000)

## ----eval=F--------------------------------------------------------------------------------------------------------------------
#  head(which(x[,"y2010","Solar photovoltaic"]>1000))

## ----eval=FALSE----------------------------------------------------------------------------------------------------------------
#  x <- x[c("CHN"),,,invert=TRUE]# exlcudes CHN
#  x <- x[c("CHN"),"2010",,invert=TRUE]# excludes CHN only for the year 2010

## ------------------------------------------------------------------------------------------------------------------------------
#  # Display all variable names containing the word "Hydro" and "hydro"
#  getNames(x[,,c("hydro","Hydro"),pmatch=T])

## ------------------------------------------------------------------------------------------------------------------------------
#  x <- x[,,grep("ydro",getNames(x),fixed = FALSE)]# fixed=F or T decides how strict should be the matching

## ------------------------------------------------------------------------------------------------------------------------------
#  x  <- toolCountryFill(x,fill=0)

## ----eval=FALSE----------------------------------------------------------------------------------------------------------------
#  x <- readSource("IRENA",subtype ="Capacity")
#  x <- time_interpolate(x,1999,integrate_interpolated_years =T,extrapolation_type = "linear" )

## ----eval=FALSE----------------------------------------------------------------------------------------------------------------
#  x <- add_columns(x,addnm = "Wind_total", dim = 3.1)

## ------------------------------------------------------------------------------------------------------------------------------
#  x_tmp <- add_dimension(x,dim = 3.1,add = "Source", nm="IRENA")

## ---- eval=FALSE,error=FALSE---------------------------------------------------------------------------------------------------
#  x <- readSource("IRENA",subtype = "Capacity",convert = FALSE) # will run readIRENA() but not convertIRENA()

## ----enablecache,eval=FALSE----------------------------------------------------------------------------------------------------
#  setConfig(enablecache = TRUE)# by default

## ----forcecache,eval=FALSE-----------------------------------------------------------------------------------------------------
#  setConfig(forcecache = c("calcIO-subtype_input","calcIO-subtype_output","calcIO-subtype_output_EDGE_buildings","calcIO-subtype_output_EDGE", "readRCPAviationShipping","convertRCPWaste","convertIEACHPreport"))

## ------------------------------------------------------------------------------------------------------------------------------
#  setConfig(enablecache = FALSE)

