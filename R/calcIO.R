#' Calc Input Output
#'
#' Computes IEA-based model data for different "subtypes" by use of raw IEA "Energy Balances" data
#' and a mapping that corresponds to the structure of "products" and "flows" of IEA.
#'
#' Mapping structure example: IEA product ANTCOAL used for IEA flow TPATFUEL, contributes via REMIND technology
#' coaltr for generating sesofos from pecoal (REMIND names)
#'
#' When using subtype `output_Industry_subsectors`, additional corrections are
#' applied to the IEA data in [`tool_fix_IEA_data_for_Industry_subsectors`].
#'
#' @md
#' @param subtype Data subtype. See default argument for possible values.
#' @return IEA data as MAgPIE object aggregated to country level
#' @author Anastasis Giannousakis
#' @seealso \code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' a <- calcOutput("IO", subtype = "output")
#' }
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% filter mutate
#' @importFrom tidyr unite
#' @importFrom tidyselect all_of
calcIO <- function(subtype = c("input", "output", "output_biomass", "trade",
                               "input_Industry_subsectors", "output_Industry_subsectors", "IEA_output", "IEA_input")) {
  subtype <- match.arg(subtype)
  switch(
    subtype,
    input = {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_inputs.csv", returnPathOnly = TRUE, where = "mappingfolder")
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")
    },
    output = {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", returnPathOnly = TRUE, where = "mappingfolder")
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")
    },
    output_biomass = {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", returnPathOnly = TRUE, where = "mappingfolder")
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")
    },
    trade = {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_trade.csv", returnPathOnly = TRUE, where = "mappingfolder")
      target <- c("REMINDitems_enty", "REMINDitems_trade")
    },
    input_Industry_subsectors = {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_inputs_Industry_subsectors.csv",
                                returnPathOnly = TRUE, where = "mappingfolder")
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")
    },
    output_Industry_subsectors = {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs_Industry_subsectors.csv",
                                returnPathOnly = TRUE, where = "mappingfolder")
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")
    },
    IEA_output = {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", returnPathOnly = TRUE, where = "mappingfolder")
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech", "iea_product", "iea_flows")
    },
    IEA_input = {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_inputs.csv", returnPathOnly = TRUE, where = "mappingfolder")
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech", "iea_product", "iea_flows")
    }
  )

  # read in data and convert from ktoe to EJ
  data <- readSource("IEA", subtype = "EnergyBalances") * 4.1868e-5

  ieamatch <- read.csv2(mapping, stringsAsFactors = FALSE, na.strings = "")

  # add total buildings electricity demand (feelb = feelcb + feelhpb + feelrhb)
  if (subtype %in% c("output", "IEA_output")) {
    ieamatch <- rbind(ieamatch,
                      ieamatch %>%
                        filter(.data$REMINDitems_out %in% c("feelcb", "feelhpb", "feelrhb")) %>%
                        mutate(REMINDitems_out = "feelb"))
  }

  if (subtype == "output_Industry_subsectors") {
    # apply corrections to IEA data to cope with fragmentary time series
    names_data_before <- getNames(data)
    data <- tool_fix_IEA_data_for_Industry_subsectors(data, ieamatch,
                                                      threshold = 1e-2)
    # warn if dimensions not present in the mapping have been added to the data
    new_product_flows <- tibble(
      text = setdiff(getNames(data), names_data_before)) %>%
      separate('text', c('product', 'flow'), sep = '\\.') %>%
      anti_join(
        ieamatch %>%
          as_tibble() %>%
          select(product = 'iea_product', flow = 'iea_flows'),

        c('product', 'flow')
      ) %>%
      unite('text', c('product', 'flow'), sep = '.') %>%
      pull('text')

    if (!is_empty(new_product_flows)) {
      warning('Product/flow combinations not present in mapping added by ',
           'fix_IEA_data_for_Industry_subsectors():\n',
           paste(new_product_flows, collapse = '\n'))
    }

    # FIXME remove product/flow combinations from the mapping that have been
    # removed from the data while replacing coke oven and blast furnace outputs
    ieamatch <- ieamatch %>%
      as_tibble() %>%
      filter(paste(.data$iea_product, .data$iea_flows, sep = '.')
             %in% getNames(data))
  }

  # delete NAs rows
  ieamatch <- ieamatch %>%
    as_tibble() %>%
    select(all_of(c("iea_product", "iea_flows", "Weight", target))) %>%
    na.omit() %>%
    unite("target", all_of(target), sep = ".", remove = FALSE) %>%
    unite('product.flow', c('iea_product', 'iea_flows'),sep = '.') %>%
    filter(!!sym("product.flow") %in% getNames(data))
  magpieNames <- ieamatch[["target"]] %>% unique()

  if (subtype == "output_biomass") {
    magpieNames <- grep("(fesob|fesoi)", magpieNames, value = TRUE)
    if (is.null(magpieNames)) {
      stop("Please consider the split between traditional and modern biomass when changing the IEA mappings.calcIO, ",
           "subtypes = output_biomass and output_EDGE_buildings")
    }
  }

  reminditems <-  do.call(
    mbind,
    lapply(magpieNames,
           function(item) {
             product_flow <- ieamatch %>%
               filter(item == .data$target) %>%
               pull('product.flow')

             weights <- ieamatch %>%
               filter(item == .data$target) %>%
               pull('Weight') %>%
               as.numeric()

             tmp <- dimSums(  data[,,product_flow]
                            * setNames(as.magpie(weights), product_flow),
                            dim = 3, na.rm = TRUE)
             getNames(tmp) <- item

             return(tmp)
             })
  )

  # Split residential Biomass into traditional and modern biomass depending upon the income per capita
  if (subtype %in% c("output", "input", "output_Industry_subsectors")) {
    # In order to split the REMIND technology biotr between biotr and biotrmod,
    # We use the traditional biomass split for EDGE buildings and divide by the total quantity of FE biomass

    edgeBio <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE)
    feBio <- calcOutput("IO", subtype = "output_biomass", aggregate = FALSE)
    shareBiotrad <- edgeBio[, , "biotrad"] / (feBio[, , "sesobio.fesob.tdbiosob"] + feBio[, , "sesobio.fesoi.tdbiosoi"])
    shareBiotrad[is.na(shareBiotrad)] <- 0
    reminditems <- mbind(reminditems,
                         setNames(reminditems[, , "pebiolc.sesobio.biotr"] * (1 - shareBiotrad),
                                  "pebiolc.sesobio.biotrmod"))
    reminditems[, , "pebiolc.sesobio.biotr"] <- reminditems[, , "pebiolc.sesobio.biotr"] * (shareBiotrad)
  }

  if (subtype == "trade") {
    # adjust the inconsistent trade data from IEA for JPN
    reminditems["JPN", 2005, "peoil.Mport"] <- reminditems["JPN", 2005, "peoil.Mport"] - 0.0245 / 31.71e-03
  }

  # replace IEA data for 1st generation biomass with data that also MAgPIE uses
  if (subtype %in% c("input", "output", "output_Industry_subsectors")) {
    bio1st <- calcOutput("1stBioDem", subtype = "ethanol_oils", aggregate = FALSE) / 1000 # PJ to EJ
    reminditems[, , "pebios.seliqbio.bioeths"] <-
      time_interpolate(bio1st[, , "pebios"], interpolated_year = getYears(reminditems),
                       integrate_interpolated_years = FALSE, extrapolation_type = "constant")
    reminditems[, , "pebioil.seliqbio.biodiesel"] <-
      time_interpolate(bio1st[, , "pebioil"], interpolated_year = getYears(reminditems),
                       integrate_interpolated_years = FALSE, extrapolation_type = "constant")
  }

  if (subtype %in% c("input", "output")) {
    # re-calculating fepet and fedie final energy based on updated EDGE shares
    share <- readSource(type = "EDGETransport", subtype = "shares_LDV_transport")
    regions <- getItems(share, dim = 1.1)
    feShares <- new.magpie(cells_and_regions = regions, years = intersect(getYears(share), getYears(reminditems)),
                           names = c("seliqfos.fepet.tdfospet", "seliqbio.fepet.tdbiopet", "seliqfos.fedie.tdfosdie",
                                     "seliqbio.fedie.tdbiodie"))
    feShares[regions, getYears(feShares), "fepet"] <- setNames(share[regions, getYears(feShares), "share_LDV_totliq"],
                                                               NULL)
    feShares[regions, getYears(feShares), "fedie"] <-
      (1 - setNames(share[regions, getYears(feShares), "share_LDV_totliq"], NULL))
    feTotal <- dimSums(reminditems[regions, getYears(feShares),
                                   c("seliqfos.fepet.tdfospet", "seliqbio.fepet.tdbiopet", "seliqfos.fedie.tdfosdie",
                                     "seliqbio.fedie.tdbiodie")],
                       dim = c(3.2, 3.3)) # seliqfos fedie dot
    feTransp <- new.magpie(cells_and_regions = regions,
                           years = getYears(feShares),
                           names = c("seliqfos.fepet.tdfospet", "seliqbio.fepet.tdbiopet", "seliqfos.fedie.tdfosdie",
                                     "seliqbio.fedie.tdbiodie"))
    feTransp[, , "seliqfos.fepet.tdfospet"] <-
      feShares[, , "seliqfos.fepet.tdfospet"] * setNames(feTotal[, , "seliqfos"], "seliqfos.fepet.tdfospet")
    feTransp[, , "seliqbio.fepet.tdbiopet"] <-
      feShares[, , "seliqbio.fepet.tdbiopet"] * setNames(feTotal[, , "seliqbio"], "seliqbio.fepet.tdbiopet")
    feTransp[, , "seliqfos.fedie.tdfosdie"] <-
      feShares[, , "seliqfos.fedie.tdfosdie"] * setNames(feTotal[, , "seliqfos"], "seliqfos.fedie.tdfosdie")
    feTransp[, , "seliqbio.fedie.tdbiodie"] <-
      feShares[, , "seliqbio.fedie.tdbiodie"] * setNames(feTotal[, , "seliqbio"], "seliqbio.fedie.tdbiodie")
    reminditems[, getYears(feTransp), c("seliqfos.fepet.tdfospet", "seliqbio.fepet.tdbiopet",
                                        "seliqfos.fedie.tdfosdie", "seliqbio.fedie.tdbiodie")] <-
      feTransp[, , c("seliqfos.fepet.tdfospet", "seliqbio.fepet.tdbiopet", "seliqfos.fedie.tdfosdie",
                     "seliqbio.fedie.tdbiodie")]
  }

  return(list(x = reminditems, weight = NULL, unit = "EJ",
              description = "IEA SE Output Data based on 2017 edition of IEA World Energy Balances"))
}
