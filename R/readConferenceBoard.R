#' readConferenceBoard 
#' @description Employment and GDP-related data from Conference Board https://www.conference-board.org/data/economydatabase/total-economy-database-productivity
#' @author Aman Malik 
#' @importFrom readxl read_excel
#' @return A magpie object 

readConferenceBoard <- function(){
  # Data on labour productivity
  input <- readxl::read_excel(path = "TED_1_JULY20201.xlsx",sheet = "TCB_ORIGINAL",skip = 4 )
  input <- input[,c(2,4:75)]
  input <- as.magpie(input,spatial=1)
  getRegions(input) <- gsub(pattern = "SCG","SRB",getRegions(input))
  getRegions(input) <- gsub(pattern = "CHN2","CHN",getRegions(input))# CHN1 is the China alternative and CHN2 is the China official numbers.
  #The former is based on alternative growth estimates, while the latter is based on official data
  
  input <- collapseNames(input,collapsedim = 2)
  
  
  # Data on agri as percent of GDP
  agri_percent_gdp <- read_excel(path = "agri_percent_gdp.xls",sheet = "Data",skip=3)
  agri_percent_gdp <- agri_percent_gdp[,-c(1,4)]
  agri_percent_gdp <- as.magpie(agri_percent_gdp,spatial=1,data=3)
  agri_percent_gdp <- collapseNames(agri_percent_gdp,collapsedim = 2)

  # Data on agri employment as percent of total employment
  agri_percent_emp <- read_excel(path = "agri_percent_employment.xls",skip = 4)
  agri_percent_emp <- agri_percent_emp[,-c(1,4, seq(4,35,1))]
  agri_percent_emp <- as.magpie(agri_percent_emp,spatial=1)
  #agri_percent_emp <- collapseNames(agri_percent_emp,preservedim = 1)
  
  # common years and regions so that it can be merged later
  com_regions <- intersect(getRegions(input),getRegions(agri_percent_gdp))
  com_years <- intersect(getYears(input),getYears(agri_percent_emp))
  input <- input[com_regions,com_years,]
  agri_percent_gdp <- agri_percent_gdp[com_regions,com_years,]
  agri_percent_emp <- agri_percent_emp[com_regions,com_years]
  
 x <-  mbind(agri_percent_emp,agri_percent_gdp,input)
 x <- add_columns(x,addnm = "Employment in agriculture",dim = 3.1)
 x <- add_columns(x,addnm = "Agriculture GDP",dim = 3.1)
 x <- add_columns(x,addnm = "Output per person (agriculture)",dim = 3.1) 
 x <- add_columns(x,addnm = "Output per person (without agriculture)",dim=3.1)
 x[,,"Employment in agriculture"] <- x[,,"Employment"]*(x[,,"Employment in agriculture (% of total employment) (modeled ILO estimate)"]/100) 
 x[,,"Agriculture GDP"] <- x[,,"GDP EKS"]*(x[,,"Agriculture, forestry, and fishing, value added (% of GDP)"]/100)
 x[,,"Output per person (agriculture)"] <- (x[,,"Agriculture GDP"]*1000)/x[,,"Employment in agriculture"]
 x[,,"Output per person (without agriculture)"] <- (x[,,"GDP EKS"]-x[,,"Agriculture GDP"])*1000/(x[,,"Employment"]-x[,,"Employment in agriculture"])

 x <- x[,2015:2019,c("Output per person (without agriculture)","Output per Employed Person","Employment in agriculture")]
 x <- magpiesort(x)
 x <- x[c("SYR","VEN","LBY"),,invert=T]

 # for all NA values, use last available value
for (i in getRegions(x)){
  for (j in getYears(x,as.integer = T)){
    for (k in getNames(x))
    if(is.na(x[i,j,k]))
      {x[i,j,k] <- x[i,j-1,k]}
  }
}
 
  return (x)
}
