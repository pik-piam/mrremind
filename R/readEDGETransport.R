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
#' @importFrom data.table rbindlist fread setcolorder := setnames
#'

readEDGETransport <- function(subtype = "logit_exponent") {
  ## mask variable for code checks
  vehicle_type <- NULL
  EDGE_scenario <- NULL
  GDP_scenario <- NULL
  value <- NULL

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
           setcolorder(tmp_dfs,c("GDP_scenario","EDGE_scenario","sector","subsector_L3","subsector_L2","subsector_L1","vehicle_type","varname","logitexp"))
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp_dfs$EDGE_scenario)) {
             tmp_EDGE <- tmp_dfs[EDGE_scenario == j]
             for (i in unique(tmp_dfs$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, datacol = 9)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }

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
           setcolorder(tmp_dfs, c("GDP_scenario", "EDGE_scenario", "iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "sw"))
           setnames(tmp_dfs, old ="sw", new ="value")

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp_dfs$EDGE_scenario)) {
             tmp_EDGE <- tmp_dfs[EDGE_scenario == j]
             for (i in unique(tmp_dfs$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }

         },


         "pref" = {
           tmp = list.files(path="./", pattern = subtype)
           tmp_dfs <- stats::setNames(object = lapply(tmp, fread), nm = sub("\\..*","",tmp))

           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }

           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)

           ## NAs in categories meant to be empty should be replaced
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"

           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp_dfs, c("GDP_scenario", "EDGE_scenario", "iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "logit_type", "varname", "value"))

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp_dfs$EDGE_scenario)) {
             tmp_EDGE <- tmp_dfs[EDGE_scenario == j]
             for (i in unique(tmp_dfs$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }

         },



         "value_time" = {
           tmp = list.files(path="./", pattern = subtype)
           tmp_dfs <- stats::setNames(object = lapply(tmp, fread), nm = sub("\\..*","",tmp))

           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }

           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"

           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp_dfs, c("GDP_scenario", "EDGE_scenario", "iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "varname", "time_price"))
           setnames(tmp_dfs, old ="time_price", new ="value")

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp_dfs$EDGE_scenario)) {
             tmp_EDGE <- tmp_dfs[EDGE_scenario == j]
             for (i in unique(tmp_dfs$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }

         },

         "harmonized_intensities" = {
           tmp <- fread(paste0(subtype, ".csv"))

           tmp$varname <- subtype
           tmp$varname = gsub(".*moinputData/","",tmp$varname)

           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("GDP_scenario", "EDGE_scenario", "iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "sector_fuel", "EJ_Mpkm_final"))
           setnames(tmp, old ="EJ_Mpkm_final", new ="value")

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }

         },

         "price_nonmot" = {
           tmp <- fread(paste0(subtype, ".csv"))
           tmp$varname <- subtype
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("GDP_scenario", "EDGE_scenario", "iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "tot_price"))
           setnames(tmp, old ="tot_price", new ="value")

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }

         },
         
         "UCD_NEC_iso" = {
           tmp <- fread(paste0(subtype, ".csv"))
           
           tmp$varname <- subtype
           tmp$varname = gsub(".*moinputData/","",tmp$varname)
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("GDP_scenario", "EDGE_scenario", "iso", "year", "vehicle_type", "technology", "type", "price_component", "varname", "non_fuel_price"))
           setnames(tmp, old ="non_fuel_price", new ="value")
           
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
             
           }
           
         },
         
         "loadFactor" = {
           tmp <- fread(paste0(subtype, ".csv"))
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("GDP_scenario", "EDGE_scenario", "iso", "year", "vehicle_type", "loadFactor"))
           setnames(tmp, old ="loadFactor", new ="value")
           mdata <- as.magpie(tmp, spatial = 3, temporal = 4)
         },

         "fe2es" = {

           tmp <- fread(paste0(subtype, ".cs4r"))

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial=2, temporal=1)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }
         },

         "esCapCost" = {

           tmp <- fread(paste0(subtype, ".cs4r"))

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial=2, temporal=1)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }
         },

         "fe_demand_tech" = {

           tmp <- fread(paste0(subtype, ".cs4r"))


           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial=2, temporal=1)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }
         },
         {
           ## default
           stop(sprintf("Subtype %s is not valid for EDGETransport.", subtype))
         })

  return(mdata)
}
