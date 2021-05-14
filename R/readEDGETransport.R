#' Read EDGETransport inputs
#'
#' Read-in EDGETransport inputs csv file as magclass object
#'
#'
#' @return magpie object of EDGEtransport iterative inputs
#' @author Marianna Rottoli, Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @param subtype logit_exponents, SW, pref, value_time, harmonized_intensities, price_nonmot, UCD_NEC_iso, loadFactor, esCapCost, fe_demand_tech, shares_LDV_transport, demISO
#'
#' @examples
#' \dontrun{ a <- readSource(type="EDGETransport")
#' }
#' @importFrom magclass read.magpie
#' @importFrom data.table rbindlist fread setcolorder := setnames
#' @importFrom rmndt approx_dt
#'

readEDGETransport <- function(subtype = "logit_exponent") {
  ## mask variable for code checks
  vehicle_type <- EDGE_scenario <- GDP_scenario <- value <- year <- sharetype <- EJ_Mpkm_final <- varname <- NULL
  fuel <- region <- iso <- node <- totdem <- `.`<- category <- tall <- all_in <-NULL

  ## load the data from EDGE-T
  EDGETrData = calcOutput("EDGETrData", aggregate = F)
  switch(subtype,

         "logit_exponent" = {
           ## do not call with convert=T, there is only global data!
           tmp_dfs <- EDGETrData[[1]]$logit_params

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

         "pref" = {
           tmp_dfs <- EDGETrData[[1]]$pref_data

           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }

           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)
           ## remove empty years (combinations of region-vehicles that are not present)
           tmp_dfs <- tmp_dfs[!is.na(year)]
           ## NAs in categories meant to be empty should be replaced
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"
           tmp_dfs[, value := as.numeric(value)]
           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp_dfs, c("GDP_scenario", "EDGE_scenario", "region", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "logit_type", "varname", "value"))
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp_dfs$EDGE_scenario)) {
             tmp_EDGE <- tmp_dfs[EDGE_scenario == j]
             for (i in unique(tmp_dfs$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4, datacol = 13)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }

         },



         "value_time" = {
           tmp_dfs <- EDGETrData[[1]]$vot_data

           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }

           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"

           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp_dfs, c("GDP_scenario", "EDGE_scenario", "region", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "varname", "time_price"))
           setnames(tmp_dfs, old ="time_price", new ="value")

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp_dfs$EDGE_scenario)) {
             tmp_EDGE <- tmp_dfs[EDGE_scenario == j]
             for (i in unique(tmp_dfs$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4, datacol = 11)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }

         },

         "harmonized_intensities" = {
           tmp <- EDGETrData[[1]]$harmonized_intensities
           tmp = tmp[!is.na(EJ_Mpkm_final)]
           tmp$varname <- subtype

           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("GDP_scenario", "EDGE_scenario", "region", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "sector_fuel", "EJ_Mpkm_final"))
           setnames(tmp, old ="EJ_Mpkm_final", new ="value")
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4, datacol = 13)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }

         },

         "price_nonmot" = {
           tmp <- EDGETrData[[1]]$price_nonmot
           tmp$varname <- subtype
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("GDP_scenario", "EDGE_scenario", "region", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "tot_price"))
           setnames(tmp, old ="tot_price", new ="value")

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4, datacol = 12)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }

         },

         "UCD_NEC_iso" = {
           tmp <- EDGETrData[[1]]$UCD_NEC_iso

           tmp$varname <- subtype
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("GDP_scenario", "EDGE_scenario", "region", "year", "vehicle_type", "technology", "type", "price_component", "varname", "non_fuel_price"))
           setnames(tmp, old ="non_fuel_price", new ="value")

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4, datacol = 10)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }

           }

         },

         "loadFactor" = {
           tmp <- EDGETrData[[1]]$loadFactor
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           tmp$varname <- subtype
           setcolorder(tmp, c("GDP_scenario", "EDGE_scenario", "region", "year", "vehicle_type", "varname", "loadFactor"))
           setnames(tmp, old ="loadFactor", new ="value")
           mdata <- as.magpie(tmp, spatial = 3, temporal = 4, datacol = 7)
         },

         "fe2es" = {

           tmp <- EDGETrData[[1]]$fe2es
           tmp <- tmp[tall>1990]
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial=2, temporal=1, datacol = 6)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }
         },

         "esCapCost" = {
           tmp <- EDGETrData[[1]]$esCapCost
           tmp <- tmp[tall>1965]
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial=2, temporal=1, datacol = 6)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }
         },

         "fe_demand_tech" = {
           tmp <- EDGETrData[[1]]$fe_demand_tech
           tmp <- tmp[tall>1990]

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial=2, temporal=1, datacol = 8)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }
         },

         "demISO" = {
           tmp <- EDGETrData[[1]]$demISO
	   ## adapt database with compatible column names and values
           setnames(tmp, old = c("sector", "dem"), new = c("all_in", "value"))
           tmp[all_in == "trn_freight", all_in := "entrp_frgt_sm"]
           tmp[all_in == "trn_shipping_intl", all_in := "entrp_frgt_lo"]
           tmp[all_in == "trn_pass", all_in := "entrp_pass_sm"]
           tmp[all_in == "trn_aviation_intl", all_in := "entrp_pass_lo"]
           ## get all the needed dimensions
           map = data.table(all_in = c("entrp_frgt_lo", "entrp_frgt_sm", "entrp_frgt_sm", "entrp_frgt_sm",
                                     "entrp_frgt_sm", "entrp_pass_lo", "entrp_pass_sm", "entrp_pass_sm",
                                     "entrp_pass_sm", "entrp_pass_sm", "entrp_pass_sm"),
                           all_enty = c("fedie", "fedie", "feelt", "fegat", "feh2t", "fedie", "fedie",
                                        "feelt", "fegat", "feh2t", "fepet"),
                            all_teEs = c("te_esdie_frgt_lo", "te_esdie_frgt_sm", "te_eselt_frgt_sm",
                                        "te_esgat_frgt_sm", "te_esh2t_frgt_sm", "te_esdie_pass_lo",
                                        "te_esdie_pass_sm", "te_eselt_pass_sm","te_esgat_pass_sm",
                                        "te_esh2t_pass_sm", "te_espet_pass_sm"))
           tmp = merge(tmp, map, by = "all_in", all = TRUE, allow.cartesian = T)

           setcolorder(tmp, c("GDP_scenario", "EDGE_scenario", "iso", "all_in", "all_enty", "all_teEs","value"))

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial=3, datacol = 7)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }
         },

         "shares_LDV_transport" = {
           tmp <- EDGETrData[[1]]$shares_LDV_transport
           tmp[, varname := subtype]
           tmp = data.table::melt(tmp, id.vars = c("GDP_scenario", "EDGE_scenario", "region", "year", "varname"))
           setnames(tmp, old = "variable", new = "sharetype")
           tmp[, c("sharetype", "year") := list(as.character(sharetype), as.character(year))]
           setcolorder(tmp, c("GDP_scenario", "EDGE_scenario", "region", "year", "sharetype", "varname", "value"))

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4, datacol = 7)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }
         },


         "pm_fe_demand_EDGETbased" = {
           tmp = EDGETrData[[1]]$complexdem$FEdem
           tmp <- tmp[year>1965]
           ## extract only ConvCase and ConvCaseWise (this subtype is only needed for calibration purposes)
           tmp = tmp[grepl("ConvCase", EDGE_scenario)]
           ## convert from final energy to useful energy
           tmp[fuel == "BEV", totdem := totdem*3] ## battery electric LDV
           tmp[fuel == "FCEV" & node == "LDV", totdem := totdem*2.5] ## FCEV vehicles LDV
           tmp[fuel == "FCEV" & node == "HDV", totdem := totdem*1.5] ## FCEV vehicles HDV
           tmp[grepl("Electric", fuel) & node == "HDV", totdem := totdem*2.5] ## battery electric HDV
           ## summarize according to the CES category
           tmp = tmp[,.(value = sum(totdem)), by = .(GDP_scenario, EDGE_scenario, region, year, node)]
           ## rename the CES nodes
           tmp[node == "LDV", node := "ueLDVt"]
           tmp[node == "HDV", node := "ueHDVt"]
           tmp[node == "Electric Trains", node := "ueelTt"]
           ## extend to time steps necessary for the input demand trend
           tmp = approx_dt(tmp,
                           xdata = c(seq(1993, 2010, 1), seq(2015, 2150, 5)),
                           xcol = "year",
                           ycol = "value",
                           idxcols = c("GDP_scenario", "EDGE_scenario", "region", "node"),
                           extrapolate = TRUE)
           ## set cols order
           setcolorder(tmp, c("GDP_scenario", "EDGE_scenario", "region", "year", "node", "value"))
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (j in unique(tmp$EDGE_scenario)) {
             tmp_EDGE <- tmp[EDGE_scenario == j]
             for (i in unique(tmp$GDP_scenario)) {
               tmp_EDGE_SSP <- tmp_EDGE[GDP_scenario == i]
               tmp_EDGE_SSP <- as.magpie(tmp_EDGE_SSP, spatial = 3, temporal = 4, datacol = 6)
               mdata <- mbind(mdata, tmp_EDGE_SSP)
             }
           }
         },

         "f35_bunkers_fe" = {
           ## used only in transport complex.
           # warning: currently assumes bunkers trajectories as fixed to "gdp_SSP2.ConvCase". Therefore bunkers are assumed unchanged in all gdp scenarios.
           tmp = EDGETrData[[1]]$complexdem$iso_FEdem
           ## select only bunkers
           tmp = tmp[category == "Bunkers",]
           ## summarize according to the CES category
           tmp = tmp[,.(value = sum(totdem)), by = .(GDP_scenario, EDGE_scenario, iso, year)]
           ## extend to necessary time steps
           tmp = approx_dt(tmp,
                           xdata = seq(2005, 2150, 5),
                           xcol = "year",
                           ycol = "value",
                           idxcols = c("GDP_scenario", "EDGE_scenario", "iso"),
                           extrapolate = TRUE)
           ## create magpie object
           tmp_data <- as.magpie(tmp, spatial = 3, temporal = 4, datacol = 5)[,,"gdp_SSP2.ConvCase"]
           # for EU regions use JRC data instead
           JRC_reg <- c("MLT","EST","CYP","LVA","LTU","LUX","SVK","SVN","HRV","BGR","HUN","ROU","FIN","DNK","IRL","CZE","GRC","AUT","PRT","SWE","BEL","NLD","POL","ESP","ITA","GBR","FRA","DEU")
           JRC <- calcOutput("JRC_IDEES", subtype="Transport", aggregate = FALSE)
           JRC_bunkers <- JRC[JRC_reg,intersect(getYears(tmp_data),getYears(JRC)),"FE|Transport|Bunkers (EJ/yr)"]
           # for years after 2015 assume bunkers constant
           tmp_data[JRC_reg,getYears(tmp_data)[getYears(tmp_data, as.integer = TRUE)>2015],] <- JRC_bunkers[JRC_reg,2015,]
           # for years lower or equal to 2015 assume bunkers equal to JRC historical values
           tmp_data[JRC_reg,getYears(tmp_data)[getYears(tmp_data, as.integer = TRUE)<=2015],] <- JRC_bunkers[JRC_reg,getYears(tmp_data)[getYears(tmp_data, as.integer = TRUE)<=2015],]
           mdata <- tmp_data
         },

         {
           ## default
           stop(sprintf("Subtype %s is not valid for EDGETransport.", subtype))
         })

  return(mdata)
}
