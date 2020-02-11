#' @title readDias
#' Contains employment factors and direct jobs for coal power, employment factors and indirect jobs
#' for coal mining for EU countries. Numbers taken from Dias et al. (2018)
#' @author Aman Malik
#' @param subtype Employment factors or Employment
#' @importFrom readxl read_excel
#' @importFrom dplyr gather rename mutate select
#' @examples
#'  \dontrun{ a <- readSource(type="Dias",convert=FALE)
#' }
#' @return magpie object containing either employment or employment factors. Type of activity
#' have also been provided.

readDias <- function(subtype){
  power_plant <- NULL
  mine <- NULL
    
  if (subtype=="Employment factors"){
    # values are in Jobs/MW and only include O&M jobs
    input <- readxl::read_excel("Employment_Dias.xlsx",sheet = 1)
    input <- input[-2]
    colnames(input)[2] <- "value"
    input$tech <- "Coal"
    input$activity <- "OM"
    input$year <- 2015
    input <- input[c(1,5,3,4,2)]
    
    x <- as.magpie(input,spatial=1,temporal=2)
 
    
    return (x)
    
  }
  if(subtype=="Employment"){
    # includes direct jobs in power plants and coal mines
    input_d <- readxl::read_excel("Employment_Dias.xlsx",sheet = 2)
    input_d <- input_d %>% 
      rename(power_plant=2,mine=3) %>% 
      gather(2:3,key = "job_source",value="value") %>% 
      mutate(type="direct") %>% 
      select(1,4,2,3)
    
    # includes indirect jobs from coal mines
    input_ind <- readxl::read_excel("Employment_Dias.xlsx",sheet = 3)
    input_ind <- input_ind[-3]# considering only intra-regional jobs
    colnames(input_ind)[2] <- "value"
    input_ind$type="indirect"
    input_ind$job_source="mine"
    input_ind <- input_ind[c(1,3,4,2)]
    
    # aggregating
    input <- rbind(input_d,input_ind)
    input$period <- 2015
    input <- input[c(1,5,2,3,4)]
    
    x <- as.magpie(input,spatial=1,temporal=2,datacol=5)
   
    return (x)
  
  }
}
