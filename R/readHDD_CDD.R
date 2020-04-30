#' Read HDD CDD
#' 
#' Read-in HDD and CDD past and scenario data as magclass object
#' 
#' @param subtype Temperature threshold for computing HDD and CDD: 18, 21 or 25
#' @return magpie object HDD CDD
#' @author Antoine Levesque
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="HDD_CDD")
#' }
#' @importFrom tidyr gather_
#' @importFrom dplyr mutate_ select_
#' @importFrom quitte as.quitte

readHDD_CDD <- function(subtype = 18) {

  rcps <- c("rcp2p6", "rcp4p5", "rcp6p0", "rcp8p5")
  variables <- c("HDD", "CDD")
  ssps <- c("ssp1", "ssp2", "ssp3","ssp4","ssp5")

df_future =    
  do.call("rbind",
          do.call("rbind",
                  do.call("rbind",
                          lapply(rcps, function(rcp) {
                            lapply(variables, function(var){
                              lapply(ssps, function(ssp){
                                file = paste0("GFDL-ESM2M_",rcp,"_",var,"_",subtype,"_",ssp,".csv")
                                tmp = read.csv(file, na.strings = "--")
                                tmp <- tmp[,colSums(is.na(tmp))<nrow(tmp)] # drops all columns with only NAs
                                tmp = tmp %>% 
                                  gather_(key_col = "region", value_col = "value", colnames(tmp[2:ncol(tmp)])) %>%
                                  mutate_(scenario = ~ssp, rcp = ~rcp, variable = ~var, tlimit = ~subtype, period = ~year) %>%
                                  select_(~-year)
                                tmp = as.quitte(tmp)
                                return(tmp)
                              })
                            })
                          })
                  )))

df_past = 
  do.call("rbind",
          lapply(variables, function(var) {
            file = paste0("GSWP3_historical_",var,"_",subtype,".csv")
            ssp = "history"
            rcp = "history"
            tmp = read.csv(file, na.strings = "--")
            tmp <- tmp[,colSums(is.na(tmp))<nrow(tmp)] # drops all columns with only NAs
            tmp = tmp %>% 
              gather_(key_col = "region", value_col = "value", colnames(tmp[2:ncol(tmp)])) %>%
              mutate_(scenario = ~ssp, rcp = ~rcp, variable = ~var, tlimit = ~subtype, period = ~year) %>%
              select_(~-year)
            tmp = as.quitte(tmp)
            return(tmp)
          }))


  
 df = rbind(df_past,df_future)
  
  mdata <- as.magpie(df)
  
  return(mdata)
}  
