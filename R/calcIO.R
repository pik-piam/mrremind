#' Calc Input Output
#'
#' Computes IEA-based model data for different "subtypes" by use of raw IEA "Energy Balances" data
#' and a mapping that corresponds to the structure of "products" and "flows" of IEA.
#'
#' Mapping structure example: IEA product ANTCOAL used for IEA flow TPATFUEL, contributes via REMIND technology
#' coaltr for generating sesofos from pecoal (REMIND names)
#'
#' @md
#' @param subtype Data subtype. See default argument for possible values.
#' @param ieaVersion Release version of IEA data, either 'default'
#' (vetted and used in REMIND) or 'latest'.
#' @param corrected boolean indicating whether corrections should be applied to the data
#' after mapping
#' @return IEA data as MAgPIE object aggregated to country level
#' @author Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput("IO", subtype = "output")
#' }
#'
#' @importFrom dplyr filter mutate
calcIO <- function(subtype = c("input", "output", "output_biomass", "output_reporting", "trade"),
                   ieaVersion = "default", corrected = FALSE) {

  subtype <- match.arg(subtype)

  switch(
    subtype,
    input = {
      mapping <- toolGetMapping(type = "sectoral",
                                name = "structuremappingIO_inputs.csv",
                                where = "mrremind",
                                returnPathOnly = TRUE)
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")
    },
    output = {
      mapping <- toolGetMapping(type = "sectoral",
                                name = "structuremappingIO_outputs.csv",
                                where = "mrcommons",
                                returnPathOnly = TRUE)
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")
    },
    output_biomass = {
      mapping <- toolGetMapping(type = "sectoral",
                                name = "structuremappingIO_outputs.csv",
                                where = "mrcommons",
                                returnPathOnly = TRUE)
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")
    },
    output_reporting = {
      mapping <- toolGetMapping(type = "sectoral",
                                name = "structuremappingIO_outputs.csv",
                                where = "mrcommons",
                                returnPathOnly = TRUE)
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")
    },
    trade = {
      mapping <- toolGetMapping(type = "sectoral",
                                name = "structuremappingIO_trade.csv",
                                where = "mrremind",
                                returnPathOnly = TRUE)
      target <- c("REMINDitems_enty", "REMINDitems_trade")
    }
  )

  if (!(ieaVersion %in% c("default", "latest"))) {
    stop("Invalid parameter `ieaVersion`. Must be either 'default' or 'latest'")
  }

  ieaSubtype <- if (ieaVersion == "default") "EnergyBalances" else "EnergyBalances-latest"

  # read in data and convert from ktoe to EJ
  data <- readSource("IEA", subtype = ieaSubtype) * 4.1868e-5

  ieamatch <- utils::read.csv2(mapping, stringsAsFactors = FALSE, na.strings = "")

  # add total buildings electricity demand (feelb = feelcb + feelhpb + feelrhb)
  # TODO: is this still needed?
  if (subtype %in% c("output", "output_reporting")) {
    ieamatch <- rbind(ieamatch,
                      ieamatch %>%
                        filter(.data$REMINDitems_out %in% c("feelcb", "feelhpb", "feelrhb")) %>%
                        mutate(REMINDitems_out = "feelb"))
  }

  # filter items starting with x_, as they are not used in REMIND, but only for reporting
  if (subtype == "output") {
    ieamatch <- ieamatch %>%
      filter(!grepl("^x_", .data$REMINDitems_in))
  }


  # delete NAs rows
  ieamatch <- ieamatch %>%
    as_tibble() %>%
    select("iea_product", "iea_flows", "Weight", target) %>%
    stats::na.omit() %>%
    tidyr::unite("target", tidyselect::all_of(target), sep = ".", remove = FALSE) %>%
    tidyr::unite("product.flow", c("iea_product", "iea_flows"), sep = ".") %>%
    filter(.data$`product.flow` %in% getNames(data))

  magpieNames <- unique(ieamatch$target)

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
               pull("product.flow")

             weights <- ieamatch %>%
               filter(item == .data$target) %>%
               pull("Weight") %>%
               as.numeric()

             tmp <- dimSums(data[, , product_flow]
                            * setNames(as.magpie(weights), product_flow),
                            dim = 3, na.rm = TRUE)
             getNames(tmp) <- item

             return(tmp)
           })
  )

  if (corrected) {

    if (subtype == "trade") {
      # adjust the inconsistent trade data from IEA for JPN
      reminditems["JPN", 2005, "peoil.Mport"] <- reminditems["JPN", 2005, "peoil.Mport"] - 0.0245 / 31.71e-03
    }

    if (subtype %in% c("output", "input")) {

      # Split residential Biomass into traditional and modern biomass depending upon the income per capita ----

      # In order to split the REMIND technology biotr between biotr and biotrmod,
      # We use the traditional biomass split for EDGE buildings and divide by the total quantity of FE biomass

      edgeBio <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings",
                            ieaVersion = ieaVersion, aggregate = FALSE)
      feBio <- calcOutput("IO", subtype = "output_biomass", ieaVersion = ieaVersion, aggregate = FALSE)
      shareBiotrad <- edgeBio[, , "biotrad"] / (feBio[, , "sesobio.fesob.tdbiosob"] + feBio[, , "sesobio.fesoi.tdbiosoi"])
      shareBiotrad[is.na(shareBiotrad)] <- 0
      reminditems <- mbind(reminditems,
                           setNames(reminditems[, , "pebiolc.sesobio.biotr"] * (1 - shareBiotrad),
                                    "pebiolc.sesobio.biotrmod"))
      reminditems[, , "pebiolc.sesobio.biotr"] <- reminditems[, , "pebiolc.sesobio.biotr"] * (shareBiotrad)

      # replace IEA data for 1st generation biomass with data that also MAgPIE uses ----
      bio1st <- calcOutput("1stBioDem", subtype = "ethanol_oils", aggregate = FALSE) / 1000 # PJ to EJ
      reminditems[, , "pebios.seliqbio.bioeths"] <-
        time_interpolate(bio1st[, , "pebios"], interpolated_year = getYears(reminditems),
                         integrate_interpolated_years = FALSE, extrapolation_type = "constant")
      reminditems[, , "pebioil.seliqbio.biodiesel"] <-
        time_interpolate(bio1st[, , "pebioil"], interpolated_year = getYears(reminditems),
                         integrate_interpolated_years = FALSE, extrapolation_type = "constant")

      # re-calculating fepet and fedie final energy based on updated EDGE shares ----
      share <- calcOutput("LDVShares", warnNA = FALSE, aggregate = FALSE)
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
  }

  desc <- paste0("IEA SE Output Data based on ", toolGetIEAYear(ieaVersion), " edition of IEA World Energy Balances")

  return(list(
    x = reminditems,
    weight = NULL,
    unit = "EJ",
    description = desc)
  )
}
