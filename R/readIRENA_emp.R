#' @title readIRENA_emp
#' The function reads in employment numbers from various reports published in the IRENA Annual Job review
#' @author Aman Malik
#' @importFrom tidyr gather
#' @importFrom dplyr filter group_by arrange summarise ungroup bind_rows mutate

readIRENA_emp <- function(){
  tech <- NULL
  region <- NULL
  value <- NULL
  
  input <- lapply(readxl::excel_sheets("Employment_IRENA.xlsx"), readxl::read_excel, path = "Employment_IRENA.xlsx")
  input_f <- data.frame(tech=NA, value=NA, region=NA,period=NA)[numeric(0), ]
  for (i in 1:6){
  input1 <- input[[i]]
  input1 <- input1 %>% gather(2:length(input[[i]]),key = "region",value = "value")
  input_world <- input1 %>% filter(region=="World") %>% arrange(tech)
  input_ROW <- input1 %>% 
      filter(region!="World") %>% 
      group_by(tech) %>% 
      summarise(value=sum(value,na.rm = T)) %>% 
      ungroup() %>% 
    arrange(tech) %>% 
    mutate(value= input_world$value-value) %>% 
    mutate(region="ROW")
    input1 <- bind_rows(input1,input_ROW) %>% mutate(period=2013+i)
    input_f <- bind_rows(input_f,input1)
  }
  input_f <- input_f[c(3,4,1,2)]
  x <- as.magpie(input_f,spatial=1,temporal=2,datacol=4)
  
  return (x)
}
   
  