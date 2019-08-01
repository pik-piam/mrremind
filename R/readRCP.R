#' Read RCP 
#' Read in RCP data
#' 
#' @return magpie object of the RCP data
#' @author Julian Oeser
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="RCP", subtype="Waste")
#' }
#' 
#' @param subtype Either 'Waste' or 'AviationShipping'
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter_ mutate_ rename_ select_ recode
#' @importFrom tidyr gather_ separate_
#' @importFrom stats complete.cases

readRCP <- function(subtype) {
  
  
  dat <- as.data.frame(do.call(rbind, lapply(list.files(pattern = ".xls$"), read_excel)))
  
  if (subtype=="Waste") {
    dat <- dat %>% 
      filter_(~Region %in% c("R5ASIA", "R5LAM", "R5MAF", "R5OECD", "R5REF")) %>%
      filter_(~grepl("Waste", Variable)) %>%
      gather_("year", "value", gather_cols=setdiff(colnames(dat),c("Region","Scenario","Variable", "Unit"))) %>%
      mutate_(year=~as.numeric(year)) %>%
      separate_("Variable", c("type", "source"), sep=" - " ) %>%
      rename_(region=~Region, scenario=~Scenario) %>%
      select_("region", "year", "scenario", "type","source", "value")
    
    dat$region <- substr(dat$region, 3, 7)
    dat$value  <- as.numeric(dat$value)
    
  } else if (subtype=="AviationShipping") {
    dat <- dat %>% 
      filter_(~Region=="World" & grepl("Aviation|Shipping", Variable)) %>%
      gather_("year", "value", gather_cols=setdiff(colnames(dat),c("Region","Scenario","Variable", "Unit"))) %>%
      mutate_(year=~as.numeric(year)) %>%
      mutate_(Region=~"GLO") %>%
      separate_("Variable", c("type", "source"), sep=" - " ) %>%
      rename_(region=~Region, scenario=~Scenario) %>%
      select_("region", "year", "scenario", "type", "source","value")
     
      dat$value  <- as.numeric(dat$value)
    
  } else {
    stop("Invalid subtype. Must be 'Waste' or 'AviationShipping'")
  }
  
  
  
  dat$scenario <- recode(dat$scenario, 
                         `IMAGE - RCP3-PD (2.6)`="rcp26",
                         `MiniCAM - RCP 4.5` = "rcp45",
                         `AIM - RCP 6.0` = "rcp60",
                         `MESSAGE - RCP 8.5` = "rcp85")
  
  dat$source <- recode(dat$source, 
                        `Waste (landfills, wastewater, non-energy incineration)`="Waste",
                        `International Shipping`="InternationalShipping")
  
  dat$type <- recode(dat$type, 
                      `CH4 emissions`="ch4",
                      `Sulfur emissions`="so2",
                      `Black Carbon emissions`="bc",
                      `Organic Carbon Emissions`="oc",
                      `CO emissions`="CO",
                      `NOx emissions`="NOx",
                      `VOC emissions`="VOC",
                      `NH3 emissions`="nh3")
  
  dat <- dat[complete.cases(dat),]
  
  out <- as.magpie(dat, spatial=1 ,datacol=6)
  
  return(out)
  
}
