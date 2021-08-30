#' Read ILO

readILO <- function(){
  obs_value <- NULL
  input <- read.csv(file = "ilostat-2020-10-09.csv") %>% select(-2,-3,-4,-9,-10) %>% 
    filter(!is.na(obs_value)) %>% rename(Country=1,Variable=2,Year=3,Value=4,Confidence=5)
  
input$Variable <- gsub(pattern = unique(input$Variable)[1],
                       replacement = "Oil and Gas",x = input$Variable,fixed = T)

input$Variable <- gsub(pattern = unique(input$Variable)[2],
                       replacement = "Coal and Lignite",x = input$Variable,fixed = T)

input <- input[,c(1,3,4,2,5)]
x <- as.magpie(input,spatial=1,temporal=2,datacol=3)
x <- collapseNames(x)
}
