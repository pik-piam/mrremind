#' Read EDGETransport inputs
#' 
#' Read-in EDGETransport inputs csv file as magclass object
#' 
#' 
#' @return magpie object of EDGEtransport iterative inputs
#' @author Marianna Rottoli, Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @param subtype logit_exponents, SW, VOT_nonmot, intensity
#'
#' @examples
#' \dontrun{ a <- readSource(type="EDGETransport")
#' }
#' @importFrom magclass read.magpie
#' @importFrom data.table rbindlist fread setcolorder
#' 

readEDGETransport <- function(subtype = "logit_exponent") {
  ## mask variable for code checks
  vehicle_type <- NULL
  switch(subtype,
         
         "logit_exponent" = {
           ## do not call with convert=T, there is only global data!
           
           tmp <- list.files(path="./", pattern = subtype)
           tmp_dfs <- stats::setNames(
                               object = lapply(tmp, fread),
                               nm = sub("\\..*","", tmp))
           
           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }
           
           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"
           
           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           
           setnames(tmp_dfs, old = "logit.exponent", new = "logitexp")
           mlogitexp <- as.magpie(tmp_dfs, datacol = 6)
           
           mdata <- mlogitexp
          
    
         },
         
         "SW" = {
           tmp = list.files(path="./", pattern = subtype)
           tmp_dfs <- stats::setNames(object = lapply(tmp, fread), nm = sub("\\..*","",tmp))
           
           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }
           
           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"
           
           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp_dfs, c("iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "sw"))
           setnames(tmp_dfs, old ="sw", new ="value")
           
           mdata <- as.magpie(tmp_dfs, datacol = 10, spatial = 1, temporal = 2)
           
         },
         
         "value_time" = {
           pattern <- "value_time"
           tmp = list.files(path="./", pattern = pattern)
           tmp_dfs <- stats::setNames(object = lapply(tmp, fread), nm = sub("\\..*","",tmp))
           
           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }
           
           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"
           
           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp_dfs, c("iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "varname", "time_price"))
           setnames(tmp_dfs, old ="time_price", new ="value")
           
           mdata <- as.magpie(tmp_dfs, datacol = 9, spatial = 1, temporal = 2)

         },
         
         "harmonized_intensities" = {
           
           tmp <- fread(paste0(subtype, ".csv"))
           tmp$varname <- subtype
           
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "sector_fuel", "EJ_Mpkm_final"))
           setnames(tmp, old ="EJ_Mpkm_final", new ="value")
           
           mdata <- as.magpie(tmp, datacol = 11, spatial = 1, temporal = 2)

         },
         
         "price_nonmot" = {
    
           tmp <- fread(paste0(subtype, ".csv"))
           tmp$varname <- subtype
           
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "tot_price"))
           setnames(tmp, old ="tot_price", new ="value")
           
           mdata <- as.magpie(tmp, datacol = 10, spatial = 1, temporal = 2)
           
         },
         
         "UCD_NEC_iso" = {
           tmp <- fread(paste0(subtype, ".csv"))
           
           tmp$varname <- subtype
           
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "non_fuel_price"))
           setnames(tmp, old ="non_fuel_price", new ="value")
           
           mdata <- as.magpie(tmp, datacol = 10, spatial = 1, temporal = 2)
         },
         
         "fe2es" = {
           tmp <- fread(paste0(subtype, ".cs4r"))
           mdata <- as.magpie(tmp, datacol=4, spatial=2, temporal=1)
         },
         
         "esCapCost" = {
           tmp <- fread(paste0(subtype, ".cs4r"))
           mdata <- as.magpie(tmp, datacol=4, spatial=2, temporal=1)
         },
         
         "demand_tech" = {
           tmp <- fread(paste0(subtype, ".cs4r"))
           mdata <- as.magpie(tmp, datacol=7, spatial=2, temporal=1)
         }, 
         {
           ## default
           stop(sprintf("Subtype %s is not valid for EDGETransport.", subtype))
         })
  
  return(mdata)
}
