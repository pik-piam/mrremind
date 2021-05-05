#' Calc Input Output
#' 
#' Computes IEA-based model data for different "subtypes" by use of raw IEA "Energy Balances" data
#' and a mapping that corresponds to the structure of "products" and "flows" of IEA.
#' 
#' Mapping structure example: IEA product ANTCOAL used for IEA flow TPATFUEL, contributes via REMIND technology
#' coaltr for generating sesofos from pecoal (REMIND names)
#' 
#' When using subtype \code{output_Industry_subsectors}, additional correctons 
#' are applied to the IEA data in 
#' \code{\link{fix_IEA_data_for_Industry_subsectors}}.
#' 
#' @param subtype data subtype. Valid subtypes are 'input', 'output_EDGE', 
#'                'trade', 'output_EDGE_buildings', 'output' and 
#'                'output_Industry_subsectors'
#' @return IEA data as MAgPIE object aggregated to country level
#' @author Anastasis Giannousakis
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput("IO", subtype = "output")
#' }
#' 
#' @importFrom dplyr %>% 
#' @importFrom tidyr unite_

calcIO <- function(subtype) {
  
  if(subtype=="input") {
    mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_inputs.csv", returnPathOnly = TRUE)
    target = c("REMINDitems_in","REMINDitems_out","REMINDitems_tech")
  } else if(subtype=="output"){
    mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", returnPathOnly = TRUE)
    target = c("REMINDitems_in","REMINDitems_out","REMINDitems_tech")
  } else if(subtype=="output_biomass"){
    mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", returnPathOnly = TRUE)
    target = c("REMINDitems_in","REMINDitems_out","REMINDitems_tech")
  } else if(subtype=="trade"){
    mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_trade.csv", returnPathOnly = TRUE)
    target = c("REMINDitems_enty","REMINDitems_trade")
  } else if(subtype=="output_EDGE"){
    mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", returnPathOnly = TRUE)
    target = c("EDGEitems")
  } else if(subtype=="output_EDGE_buildings"){
    mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", returnPathOnly = TRUE)
    target = c("EDGE_buildings")
  } else if ('input_Industry_subsectors' == subtype) {
    mapping <- toolGetMapping(
      type = 'sectoral',
      name = 'structuremappingIO_inputs_Industry_subsectors.csv',
      returnPathOnly = TRUE)
    target <- c('REMINDitems_in', 'REMINDitems_out', 'REMINDitems_tech')
  } else if ('output_Industry_subsectors' == subtype) {
    mapping <- toolGetMapping(
      type = 'sectoral',
      name = 'structuremappingIO_outputs_Industry_subsectors.csv',
      returnPathOnly = TRUE)
    target <- c('REMINDitems_in', 'REMINDitems_out', 'REMINDitems_tech')
  } else if ('IEA_output' == subtype) {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", returnPathOnly = TRUE)
      target = c("REMINDitems_in","REMINDitems_out","REMINDitems_tech","iea_product","iea_flows")
  } else if ('IEA_input' == subtype) {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_inputs.csv", returnPathOnly = TRUE)
      target = c("REMINDitems_in","REMINDitems_out","REMINDitems_tech","iea_product","iea_flows")
  }else { stop("valid subtypes are 'input', 'output_EDGE', 'trade', 'output_EDGE_buildings', 'output_biomass' and 'output'")} 
  
  # read in data and convert from ktoe to EJ
  data <- readSource("IEA",subtype="EnergyBalances") * 0.0000418680000
  
  ### calculate data
  ieamatch <- read.csv2(mapping, stringsAsFactors = FALSE, na.strings ="" )
  regions  <- getRegions(data)
  years    <- getYears(data)
  #delete NAs rows
  ieamatch = ieamatch[c("iea_product","iea_flows",target,"Weight")] %>% na.omit()
  #
  ieamatch = ieamatch %>% unite_("target",target, sep = ".")
  magpnames = ieamatch[["target"]]
  magpnames <- unique(magpnames)
  
  if ('output_Industry_subsectors' == subtype) {
    # apply corrections to IEA data to cope with fragmentary time series
    data <- fix_IEA_data_for_Industry_subsectors(data, ieamatch)
  }
  if (subtype == "output_biomass"){
    magpnames = grep("(fesob|fesoi)",magpnames, value = T)
    if (is.null(magpnames)){
      stop("Please consider the split between traditional and modern biomass when changing the IEA mappings.calcIO, subtypes = output_biomass and output_EDGE_buildings")
    }
  }

  ## in case we include IEA categories in the output, iea categories in `ieamatch` got renamed
  ieapname <- "iea_product"
  ieafname <- "iea_flows"
  if(subtype %in% c("IEA_output", "IEA_input")){
    ieapname <- "iea_product.1"
    ieafname <- "iea_flows.1"
  }
  
  reminditems =  do.call(mbind,
                        lapply(magpnames, function(item) {
                          testdf = ieamatch[ieamatch$target == item ,c(ieapname,ieafname,"Weight")]
                          prfl <- paste(testdf[,ieapname],testdf[,ieafname],sep=".")
                          vec <- as.numeric(ieamatch[rownames(testdf),"Weight"])
                          names(vec) <- prfl
                          tmp <- data[,,prfl] * as.magpie(vec)
                          tmp <- dimSums(tmp,dim=3,na.rm = TRUE)
                          getNames(tmp) <- item
                          return(tmp)
                          
                        })
 )
  
  #Split residential Biomass into traditional and modern biomass depending upon the income per capita
  if (subtype %in% c("output_EDGE", "output_EDGE_buildings")){
    
   if (subtype ==  "output_EDGE"){
     n_biotrad = "feresbiotrad"
     n_biomod = "feresbiomod"
     n_bioshare = "feresbioshare"
   } else if (subtype == "output_EDGE_buildings"){
     n_biotrad = "biotrad"
     n_biomod = "biomod"
     n_bioshare = "bioshare"
   }
  #Read-in data to compute income per capita
    gdp <- calcOutput("GDPpppPast", aggregate = F)
    pop <- calcOutput("PopulationPast", aggregate = F)
    gdppop <- gdp[,intersect(getYears(gdp),getYears(pop)),]/pop[,intersect(getYears(gdp),getYears(pop)),]  
    #Create a lambda which is 1 for income per capita <= 10000, and 0 above 15000
    #        pmin( 1          , pmax(0       , (15000 - gdppop)/(15000 - 10000))). the multiplication by gdppop was necessary to avoid error from vector length.
  lambda = pmin(gdppop *0 +1, pmax(0*gdppop, (15000 - gdppop)/(15000-10000)))
  lambda = time_interpolate(lambda,getYears(reminditems), extrapolation_type = "constant")
  
  #Split Bioshare (residential PRIMSBIO) between traditional and modern biomass according to lambda
  bioshareTrad = setNames(reminditems[,,n_bioshare] * lambda, n_biotrad)
  bioshareMod = setNames(reminditems[,,n_bioshare] - bioshareTrad, n_biomod)
  
  #In case biomod and biotrad do not exist yet in the data set, create dummy items
  if (! any(n_biomod %in% getNames(reminditems))){
    reminditems = mbind(reminditems,
                        setNames(reminditems[,,n_bioshare] * 0, n_biomod))
  }
  if (! any(n_biotrad %in% getNames(reminditems))){
    reminditems = mbind(reminditems,
                        setNames(reminditems[,,n_bioshare] * 0, n_biotrad))
  }

  #Add the values from bioshare to the other modern and traditional biomass
  reminditems[,, n_biotrad] = reminditems[,, n_biotrad] + bioshareTrad
  reminditems[,, n_biomod] = reminditems[,, n_biomod] + bioshareMod

  #Remove the bioshare item
  reminditems = reminditems[,,n_bioshare, invert = T]

  } else if (subtype %in% c("output","input", "output_Industry_subsectors", "input_Industry_subsectors")){
    #In order to split the REMIND technology biotr between biotr and biotrmod,
    # We use the traditional biomass split for EDGE buildings and divide by the total quantity of FE biomass

    edge_bio = calcOutput("IO",subtype = "output_EDGE_buildings", aggregate = F)
    fe_bio = calcOutput("IO", subtype = "output_biomass", aggregate = F)
    share_biotrad = (edge_bio[,,"biotrad"]
                    / (fe_bio[,,"sesobio.fesob.tdbiosob"] +fe_bio[,,"sesobio.fesoi.tdbiosoi"]))
    share_biotrad[is.na(share_biotrad)] <- 0
    reminditems = mbind(reminditems,
                        setNames(reminditems[,,"pebiolc.sesobio.biotr"] * (1-share_biotrad),
                                 "pebiolc.sesobio.biotrmod"))
    reminditems[,,"pebiolc.sesobio.biotr"] = reminditems[,,"pebiolc.sesobio.biotr"] * (share_biotrad)

  }


  if(subtype=="trade") {
     # adjust the inconsistent trade data from IEA for JPN
     reminditems["JPN",2005,"peoil.Mport"] <- reminditems["JPN",2005,"peoil.Mport"] - 0.0245/31.71e-03
  }

  # replace IEA data for 1st generation biomass with data that also MAgPIE uses
  if(subtype == "input") {
    bio1st <- calcOutput("1stBioDem", subtype="ethanol_oils",aggregate = FALSE) / 1000 # PJ -> EJ
    reminditems[,,"pebios.seliqbio.bioeths"]    <- time_interpolate(bio1st[,,"pebios"],interpolated_year = getYears(reminditems),integrate_interpolated_years = FALSE,extrapolation_type = "constant")
    reminditems[,,"pebioil.seliqbio.biodiesel"] <- time_interpolate(bio1st[,,"pebioil"],interpolated_year = getYears(reminditems),integrate_interpolated_years = FALSE,extrapolation_type = "constant")
  }

  # replace IEA data for 1st generation biomass with data that also MAgPIE uses
  if(subtype %in% c("output", 'output_Industry_subsectors')) {
    bio1st <- calcOutput("1stBioDem", subtype="ethanol_oils",aggregate = FALSE) / 1000 # PJ -> EJ
    reminditems[,,"pebios.seliqbio.bioeths"]    <- time_interpolate(bio1st[,,"pebios"],interpolated_year = getYears(reminditems),integrate_interpolated_years = FALSE,extrapolation_type = "constant")
    reminditems[,,"pebioil.seliqbio.biodiesel"] <- time_interpolate(bio1st[,,"pebioil"],interpolated_year = getYears(reminditems),integrate_interpolated_years = FALSE,extrapolation_type = "constant")
  }

  # split off a 1e-4 fraction of chemicals/otherInd electricity for high
  # temperature heat electricity
  if ('output_Industry_subsectors' == subtype) {
    hth <- 1e-4
    reminditems <- mbind(
      reminditems[,,'feelwlth_', pmatch = TRUE, invert = TRUE],

      setNames(
        reminditems[,,'seel.feelwlth_chemicals.tdelwlth_chemicals'] * hth,
        'seel.feelhth_chemicals.tdelhth_chemicals'),
      reminditems[,,'seel.feelwlth_chemicals.tdelwlth_chemicals'] * (1 - hth),

      setNames(
        reminditems[,,'seel.feelwlth_otherInd.tdelwlth_otherInd'] * hth,
        'seel.feelhth_otherInd.tdelhth_otherInd'),
      reminditems[,,'seel.feelwlth_otherInd.tdelwlth_otherInd'] * (1 - hth)
    )
  }
  
  if(subtype %in% c("input", "output")){
    # re-calculating fepet and fedie final energy based on updated EDGE shares
    share <- readSource(type="EDGETransport", subtype = "shares_LDV_transport")
    feShares <- new.magpie(cells_and_regions = getRegions(share), years = intersect(getYears(share),getYears(reminditems)), names = c("seliqfos.fepet.tdfospet","seliqbio.fepet.tdbiopet","seliqfos.fedie.tdfosdie","seliqbio.fedie.tdbiodie"))
    feShares[getRegions(share),getYears(feShares),"fepet"] <- setNames(share[getRegions(share),getYears(feShares),"share_LDV_totliq"],NULL)
    feShares[getRegions(share),getYears(feShares),"fedie"] <- (1-setNames(share[getRegions(share),getYears(feShares),"share_LDV_totliq"],NULL))
    feTotal <- dimSums(reminditems[getRegions(share),getYears(feShares),c("seliqfos.fepet.tdfospet","seliqbio.fepet.tdbiopet","seliqfos.fedie.tdfosdie","seliqbio.fedie.tdbiodie")],dim=c(3.2,3.3)) #seliqfos	fedie	dot
    feTransp <- new.magpie(cells_and_regions = getRegions(share), years = getYears(feShares), names = c("seliqfos.fepet.tdfospet","seliqbio.fepet.tdbiopet","seliqfos.fedie.tdfosdie","seliqbio.fedie.tdbiodie"))
    feTransp[,,"seliqfos.fepet.tdfospet"] <- feShares[,,"seliqfos.fepet.tdfospet"]*setNames(feTotal[,,"seliqfos"],"seliqfos.fepet.tdfospet")
    feTransp[,,"seliqbio.fepet.tdbiopet"] <- feShares[,,"seliqbio.fepet.tdbiopet"]*setNames(feTotal[,,"seliqbio"],"seliqbio.fepet.tdbiopet")
    feTransp[,,"seliqfos.fedie.tdfosdie"] <- feShares[,,"seliqfos.fedie.tdfosdie"]*setNames(feTotal[,,"seliqfos"],"seliqfos.fedie.tdfosdie")
    feTransp[,,"seliqbio.fedie.tdbiodie"] <- feShares[,,"seliqbio.fedie.tdbiodie"]*setNames(feTotal[,,"seliqbio"],"seliqbio.fedie.tdbiodie")
    reminditems[,getYears(feTransp),c("seliqfos.fepet.tdfospet","seliqbio.fepet.tdbiopet","seliqfos.fedie.tdfosdie","seliqbio.fedie.tdbiodie")] <- feTransp[,,c("seliqfos.fepet.tdfospet","seliqbio.fepet.tdbiopet","seliqfos.fedie.tdfosdie","seliqbio.fedie.tdbiodie")]
  }
  
  return(list(x=reminditems,weight=NULL,unit="EJ",
              description="IEA SE Output Data based on 2017 edition of IEA World Energy Balances"))
}
