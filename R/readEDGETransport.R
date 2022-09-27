#' Read EDGETransport inputs
#'
#' Read-in EDGETransport inputs csv file as magclass object
#'
#'
#' @return magpie object of EDGEtransport iterative inputs
#' @author Marianna Rottoli, Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @param subtype logit_exponents, SW, pref, ptab4W, value_time, harmonized_intensities, price_nonmot, UCD_NEC_iso, loadFactor, esCapCost, fe_demand_tech, shares_LDV_transport, demISO, annual_mileage
#'
#' @examples
#' \dontrun{ a <- readSource(type="EDGETransport")
#' }
#' @importFrom magclass read.magpie
#' @importFrom data.table rbindlist fread setcolorder := setnames setkey
#' @importFrom rmndt approx_dt
#'

readEDGETransport <- function(subtype = "logit_exponent") {
  ## mask variable for code checks
  vehicle_type <- EDGE_scenario <- GDP_scenario <- DEM_scenario <- value <- year <- sharetype <-
    EJ_Mpkm_final <- varname <- fuel <- region <- iso <- node <- totdem <- `.`<-
      category <- tall <- all_in <- price_component <- NULL

  ## load the data from EDGE-T
  EDGETrData = calcOutput("EDGETrData", aggregate = F)
  EDGETrData_all=list()

  ## all data.tables can be combined directly
  for (i in c("fe_demand_tech", "fe2es", "esCapCost", "shares_LDV_transport",
              "demISO", "price_nonmot", "harmonized_intensities", "UCD_NEC_iso",
              "loadFactor", "annual_mileage", "ptab4W")) {
    EDGETrData_all[[i]] <- rbindlist(
      lapply(seq(1,length(EDGETrData)),
             function(x) {
               EDGETrData[[x]][[i]]
               return(EDGETrData[[x]][[i]])
             }), use.names=TRUE)
  }

  ## sub-lists have to be dealt with separately
  for (j in c("logit_params", "vot_data", "pref_data", "complexdem")) {
    for (i in names(EDGETrData[[1]][[j]])) {
      EDGETrData_all[[j]][[i]] <- rbindlist(
        lapply(seq(1,length(EDGETrData)),
               function(x) {
                 EDGETrData[[x]][[j]][[i]]
                 return(EDGETrData[[x]][[j]][[i]])
               }), use.names=TRUE)
    }
  }

  create_copy_demscens <- function(dt) {
    setkey(dt, "DEM_scenario", "GDP_scenario", "EDGE_scenario")
    ## Workaround for NAVIGATE: copy-create demand scenarios which we do not supply by EDGE-T
    dt <- rbind(dt,
                dt[c("gdp_SSP2EU", "gdp_SSP2EU", "NAV_ele"), nomatch=NULL][
                  , DEM_scenario := "gdp_SSP2EU_NAV_ele"],
                dt[c("gdp_SSP2EU", "gdp_SSP2EU", "NAV_tec"), nomatch=NULL][
                  , DEM_scenario := "gdp_SSP2EU_NAV_tec"],
                dt[c("gdp_SSP2EU_lowdem", "gdp_SSP2EU", "NAV_act"), nomatch=NULL][
                  , DEM_scenario := "gdp_SSP2EU_NAV_act"],
                dt[c("gdp_SSP2EU_lowdem", "gdp_SSP2EU", "NAV_all"), nomatch=NULL][
                  , DEM_scenario := "gdp_SSP2EU_NAV_all"]
    )
    setkey(dt, "DEM_scenario")
    dt["gdp_SSP2EU_lowdem", DEM_scenario := "gdp_SSP2_lowEn"]
    scens <- unique(dt$DEM_scenario)
    ## this scenario is not supported by REMIND
    dt <- dt[scens["gdp_SSP2EU_lowdem" != scens]]
    return(dt)
  }

  compress_magpie <- function(dt, ...) {
    dt <- create_copy_demscens(dt)
    setkey(dt, "EDGE_scenario", "DEM_scenario", "GDP_scenario")
    mdata <- NULL
    for (i in unique(dt$EDGE_scenario)) {
      for (j in unique(dt$DEM_scenario)) {
        for (k in unique(dt$GDP_scenario)) {
          tmp <- dt[.(i, j, k), nomatch=NULL]
          if(nrow(tmp) > 0) {
            mdata <- mbind(
              mdata, as.magpie(tmp, ...))
          }
        }
      }
    }
    return(mdata)

  }

  switch(subtype,

         "logit_exponent" = {
           ## do not call with convert=T, there is only global data!
           tmp_dfs <- EDGETrData_all$logit_params

           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }


           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"

           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]

           setnames(tmp_dfs, old = "logit.exponent", new = "logitexp")
           setcolorder(tmp_dfs, c("GDP_scenario", "DEM_scenario", "EDGE_scenario",
                                  "sector", "subsector_L3", "subsector_L2", "subsector_L1",
                                  "vehicle_type", "varname", "logitexp"))
           ## concatenate multiple magpie objects each one containing one
           ## SSP realization to avoid large objects
           mdata <- compress_magpie(tmp_dfs, datacol = 10)

         },

         "pref" = {
           tmp_dfs <- EDGETrData_all$pref_data

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
           setcolorder(tmp_dfs, c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "region",
                                  "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1",
                                  "vehicle_type", "technology", "logit_type", "varname", "value"))
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- compress_magpie(tmp_dfs, spatial = 4, temporal = 5, datacol = 14)

         },



         "value_time" = {
           tmp_dfs <- EDGETrData_all$vot_data

           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }

           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"

           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp_dfs, c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "region",
                                  "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1",
                                  "vehicle_type", "varname", "time_price"))
           setnames(tmp_dfs, old ="time_price", new ="value")

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- compress_magpie(tmp_dfs, spatial = 4, temporal = 5, datacol = 12)

         },

         "harmonized_intensities" = {
           tmp <- EDGETrData_all$harmonized_intensities
           tmp = tmp[!is.na(EJ_Mpkm_final)]
           tmp$varname <- subtype

           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "region", "year",
                              "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type",
                              "technology", "varname", "sector_fuel", "EJ_Mpkm_final"))
           setnames(tmp, old ="EJ_Mpkm_final", new ="value")
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- compress_magpie(tmp, spatial = 4, temporal = 5, datacol = 14)

         },

         "price_nonmot" = {
           tmp <- EDGETrData_all$price_nonmot
           tmp$varname <- subtype
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "region", "year",
                              "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type",
                              "technology", "varname", "tot_price"))
           setnames(tmp, old ="tot_price", new ="value")

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- compress_magpie(tmp, spatial = 4, temporal = 5, datacol = 13)

         },

         "UCD_NEC_iso" = {
           tmp <- EDGETrData_all$UCD_NEC_iso
           tmp[price_component == "Capital costs (purchase)", price_component := "Capital_costs_purchase"]
           tmp$varname <- subtype
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "region", "year",
                              "vehicle_type", "technology", "price_component", "varname", "non_fuel_price"))
           setnames(tmp, old ="non_fuel_price", new ="value")

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- compress_magpie(tmp, spatial = 4, temporal = 5, datacol = 10)

         },

         "loadFactor" = {
           tmp <- EDGETrData_all$loadFactor
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           tmp$varname <- subtype
           setcolorder(tmp, c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "region", "year",
                              "vehicle_type", "varname", "loadFactor"))
           setnames(tmp, old ="loadFactor", new ="value")
           mdata <- compress_magpie(tmp, spatial = 4, temporal = 5, datacol = 8)
         },

         "fe2es" = {

           tmp <- EDGETrData_all$fe2es
           tmp <- tmp[tall>1990]
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- compress_magpie(tmp, spatial = 2, temporal = 1, datacol = 7)

         },

         "esCapCost" = {
           tmp <- EDGETrData_all$esCapCost
           tmp <- tmp[tall>1965]
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- compress_magpie(tmp, spatial = 2, temporal = 1, datacol = 7)

         },

         "fe_demand_tech" = {
           tmp <- EDGETrData_all$fe_demand_tech
           tmp <- tmp[tall>1990]

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- compress_magpie(tmp, spatial = 2, temporal = 1, datacol = 9)
         },

         "demISO" = {
           tmp <- EDGETrData_all$demISO
                 ## adapt database with compatible column names and values
           setnames(tmp, old = c("sector", "tech_output"), new = c("all_in", "value"))
           tmp[all_in == "trn_freight", all_in := "entrp_frgt_sm"]
           tmp[all_in == "trn_shipping_intl", all_in := "entrp_frgt_lo"]
           tmp[all_in == "trn_pass", all_in := "entrp_pass_sm"]
           tmp[all_in == "trn_aviation_intl", all_in := "entrp_pass_lo"]
           ## TODO check if really we expect the same value for all technologies within a node (as it is now calculated)
           tmp = tmp[year == 2010]
           tmp[, value := round(value)]
           tmp = tmp[, .(value = sum(value)),
                     by = c("iso", "all_in", "GDP_scenario", "DEM_scenario", "EDGE_scenario")]
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
           tmp = tmp[, c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "iso", "all_in",
                         "all_enty", "all_teEs", "value")]

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- compress_magpie(tmp, spatial = 4, datacol = 8)
         },

         "shares_LDV_transport" = {
           tmp <- EDGETrData_all$shares_LDV_transport
           tmp[, varname := subtype]
           tmp = data.table::melt(tmp, id.vars = c("GDP_scenario", "DEM_scenario", "EDGE_scenario",
                                                   "region", "year", "varname"))
           setnames(tmp, old = "variable", new = "sharetype")
           tmp[, c("sharetype", "year") := list(as.character(sharetype), as.character(year))]
           setcolorder(tmp, c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "region", "year",
                              "sharetype", "varname", "value"))

           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- compress_magpie(tmp, spatial = 4, temporal = 5, datacol = 8)
         },


         "pm_fe_demand_EDGETbased" = {
           tmp = EDGETrData_all$complexdem$FEdem
           tmp <- tmp[year>1965]
           ## extract only Mix1 and Mix1Wise (this subtype is only needed for calibration purposes)
           tmp = tmp[grepl("Mix1", EDGE_scenario)]
           ## convert from final energy to useful energy
           tmp[fuel == "BEV", totdem := totdem*3] ## battery electric LDV
           tmp[fuel == "FCEV" & node == "LDV", totdem := totdem*2.5] ## FCEV vehicles LDV
           tmp[fuel == "FCEV" & node == "HDV", totdem := totdem*1.5] ## FCEV vehicles HDV
           tmp[grepl("Electric", fuel) & node == "HDV", totdem := totdem*2.5] ## battery electric HDV
           ## summarize according to the CES category
           tmp = tmp[,.(value = sum(totdem)), by = .(GDP_scenario, DEM_scenario, EDGE_scenario,
                                                     region, year, node)]
           ## rename the CES nodes
           tmp[node == "LDV", node := "ueLDVt"]
           tmp[node == "HDV", node := "ueHDVt"]
           tmp[node == "Electric Trains", node := "ueelTt"]
           ## extend to time steps necessary for the input demand trend
           tmp = approx_dt(tmp,
                           xdata = c(seq(1993, 2010, 1), seq(2015, 2150, 5)),
                           xcol = "year",
                           ycol = "value",
                           idxcols = c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "region", "node"),
                           extrapolate = TRUE)
           ## set cols order
           setcolorder(tmp, c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "region",
                              "year", "node", "value"))
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- compress_magpie(tmp, spatial = 4, temporal = 5, datacol = 7)
         },

         "annual_mileage" = {
           tmp <- EDGETrData_all$annual_mileage
           tmp[, varname := subtype]
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           tmp$varname <- subtype
           setcolorder(tmp, c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "region",
                              "year", "vehicle_type", "varname", "annual_mileage"))
           setnames(tmp, old ="annual_mileage", new ="value")
           mdata <- compress_magpie(tmp, spatial = 4, temporal = 5, datacol = 8)
         },

         "ptab4W" = {
           tmp <- EDGETrData_all$ptab4W
           tmp[, varname := subtype]
           tmp$varname <- subtype
           setcolorder(tmp, c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "param",
                              "varname", "value"))
           mdata <- compress_magpie(tmp, datacol = 6)
         },

         "f35_bunkers_fe" = {
           ## used only in transport complex.
           ## warning: currently assumes bunkers trajectories as fixed to the first trajectory found for "gdp_SSP2". Therefore bunkers are assumed unchanged in all gdp scenarios.
           tmp = EDGETrData_all$complexdem$iso_FEdem
           ## select only bunkers
           tmp = tmp[category == "Bunkers",]
           ## summarize according to the CES category
           tmp = tmp[,.(value = sum(totdem)), by = .(GDP_scenario, DEM_scenario, EDGE_scenario, iso, year)]
           ## extend to necessary time steps
           tmp = approx_dt(tmp,
                           xdata = seq(2005, 2150, 5),
                           xcol = "year",
                           ycol = "value",
                           idxcols = c("GDP_scenario", "DEM_scenario", "EDGE_scenario", "iso"),
                           extrapolate = TRUE)
           ## create magpie object
           tmp_data <- compress_magpie(tmp, spatial = 4, temporal = 5, datacol = 6)
           ssp2_scen <- getNames(tmp_data[,, "gdp_SSP2"])[1]
           tmp_data <- tmp_data[,, ssp2_scen]
           # for EU regions use JRC data instead
           JRC_reg <- c("MLT","EST","CYP","LVA","LTU","LUX","SVK","SVN","HRV","BGR","HUN","ROU","FIN","DNK","IRL","CZE","GRC","AUT","PRT","SWE","BEL","NLD","POL","ESP","ITA","GBR","FRA","DEU")
           JRC <- calcOutput("JRC_IDEES", subtype="Transport", aggregate = FALSE)
           JRC_bunkers <- JRC[JRC_reg,intersect(getYears(tmp_data), getYears(JRC)),
                              "FE|Transport|Bunkers (EJ/yr)"]
           # for years after 2015 assume bunkers constant
           tmp_data[JRC_reg, getYears(tmp_data)[getYears(tmp_data, as.integer = TRUE)>2015],] <-
             JRC_bunkers[JRC_reg,2015,]
           # for years lower or equal to 2015 assume bunkers equal to JRC historical values
           tmp_data[JRC_reg, getYears(tmp_data)[getYears(tmp_data, as.integer = TRUE)<=2015],] <-
             JRC_bunkers[JRC_reg,getYears(tmp_data)[getYears(tmp_data, as.integer = TRUE)<=2015],]
           mdata <- tmp_data
         },

         {
           ## default
           stop(sprintf("Subtype %s is not valid for EDGETransport.", subtype))
         })

  return(mdata)
}
