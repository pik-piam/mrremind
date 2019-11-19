#' Data from Coal Swarm on coal power plants
#' @description  Data on historical (2015-2019) operating, under-construction, planned and announced Coal Plants by country (in MW) 
#' from EndCoal.org (Actual source: Coal Swarm and Global Plant Tracker) 
#' @author Aman Malik
#' @import readxl
#' @import dplyr


# read in Data
readEndCoal <- function(){
# status of coal power plants in Jan. 2019
input_2019 <- read_excel("Global Coal Plant Tracker January 2019.xlsx",sheet = 2)# Data from January 2019
input_2019 <- input_2019 %>% select(1,5,13)
# status of all power plants from 2015 to 2019
input_ts <- read_excel("GCPT status history Jan15 - Jan19.xlsx", sheet = "Projects")
input_ts <- input_ts %>% select(1:2,7,9,11,13,15)
colnames(input_ts)[3:7] <- c(2019:2015)

joined <- left_join(input_ts,input_2019,by=c("Tracker ID","Country")) %>% # joining the two dataframes based on Tracker ID and country
  na.omit()  %>% 
  gather_(gather_cols=setdiff(colnames(input_ts),c("Tracker ID","Country")),key_col = "period",value_col="status") %>%  
  rename_(Capacity=~`Capacity (MW)`) %>% 
  mutate_(Capacity=~as.numeric(Capacity)) %>% 
  na.omit() %>% # removing all NAs generated from previous steps
  filter_(~status %in% unique(status)[1:9]) %>% # only include select status names. all others e.g., XXX are removed
  select(2:5) %>% 
  group_by_(~Country,~period,~status) %>% 
  summarise_(Capacity=~sum(Capacity)) 

x <- as.magpie(joined,spatial=1,temporal=2)
getSets(x) <- c("Region","Year","Status")
return(x)
}