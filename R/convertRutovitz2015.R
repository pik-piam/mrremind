#' @title convertRutovitz2015
#' @param x MAgPIE object to be converted
#' @param subtype "oecd_ef" or "regional_mult" or "regional_ef" or "coal_ef" or "gas_ef"
#' @author Aman Malik
#' @importFrom stats na.omit


convertRutovitz2015 <- function(x, subtype) {
  country <- NULL
  year <- NULL
  region <- NULL
  value <- NULL
  tech <- NULL
  activity <- NULL

  mapping <- toolGetMapping(type = "regional", name = "regionalmappingWEO2014.csv", where = "mappingfolder")
  colnames(mapping) <- c("region", "country")
  mapping$country <- toolCountry2isocode(mapping$country)

  if (subtype == "oecd_ef") {
    # x <- readSource(type = "Rutovitz2015",subtype = "oecd_ef",convert = FALSE)

  # oecd regions in mapping file
  oecd <- c("OECD Europe", "OECD Americas", "OECD Asia Oceania")
  # oecd countries from mapping file
  oecd_con <- mapping[mapping$region %in% oecd, ]$country


  # since x cannot directly be changed to accept OECD countries and years,
  # make a new magpie object
  oecd_ef <- new.magpie(unique(oecd_con), c(2015, 2020, 2030), names = getNames(x))
  getSets(oecd_ef) <- getSets(x)

  # assign all countries and techs, same value as x
  oecd_ef[, , ] <- x
  oecd_ef[is.na(oecd_ef)] <- 0
  x <- toolCountryFill(oecd_ef, fill = 0)


  }
  if (subtype == "regional_mult") {

  # x <- readSource(type = "Rutovitz2015",subtype = "regional_mult",convert=FALSE)
    reg_mult <- as.data.frame(x) %>%
    select(2, 3, 5) %>%
    rename(region = 1, year = 2, value = 3)

    mapping$region <- gsub("OECD Europe|OECD Asia Oceania|OECD Americas", "OECD", x = mapping$region)
    mapping$region <- gsub("Central Africa|West Africa|Southern Africa|East Africa|North Africa", "Africa", x = mapping$region)

    # assign countries to regions
    reg_mult2 <- left_join(reg_mult, mapping, by = "region")   %>%
      mutate(country = ifelse(region == "China", "CHN", country)) %>%
      mutate(country = ifelse(region == "India", "IND", country)) %>%
      filter(!(region == "Non-OECD Asia" & country == "CHN")) %>% # removing duplicates
      filter(!(region == "Non-OECD Asia" & country == "IND")) %>% # removing duplicates
      select(country, year, value)

    x <- as.magpie(reg_mult2)

    x <- toolCountryFill(x, fill = 0)
    x["CYP", , ] <- as.numeric(x["DEU", , ]) # cyprus gets oecd values

  }
  if (subtype == "regional_ef") {

    # x <- readSource(type = "Rutovitz2015",subtype = "regional_ef",convert = FALSE)

    # Changing names of column "regions" in mapping to better match both data frames
    mapping$region <- gsub("Central Africa|West Africa|Southern Africa|East Africa|North Africa", "Africa", x = mapping$region)
    getItems(x, dim = 1) <- gsub(getItems(x, dim = 1), replacement = c("OECD Americas"), pattern = c("OECD North America")) # replace OECD Americas with OECD North America


    x_df <- as.data.frame(x) %>% 
      select(2, 4, 5, 6) %>%
      rename(region = 1, tech = 2, activity = 3, value = 4) %>%
      na.omit() %>%
      left_join(mapping, by = "region") %>%
      select(country, tech, activity, value)

    x <- as.magpie(x_df, spatial = 1, temporal = NULL, datacol = 4)
    x[is.na(x)] <- 0 # for all countries with no data, replace by 0
    x <- toolCountryFill(x, fill = 0)
    getYears(x) <- 2015

  }
  if (subtype == "coal_ef") {
    # x <- readSource(type = "Rutovitz2015",subtype = "coal_ef",convert = FALSE)

    mapping$region <- gsub("Central Africa|West Africa|Southern Africa|East Africa|North Africa", "Africa", x = mapping$region)

    getItems(x, dim = 1) <- gsub("OECD North America", "OECD Americas", x = getItems(x, dim = 1))
    getItems(x, dim = 1) <- gsub("OECD Pacific", "OECD Asia Oceania", x = getItems(x, dim = 1))
    getItems(x, dim = 1) <- gsub("Developing Asia", "Non-OECD Asia", x = getItems(x, dim = 1))

    # assign countries to all regions in Rutovitz for coal employment factors
      x_df <- as.data.frame(x) %>%
      select(2, 4, 5, 6) %>%
      rename(region = 1, tech = 2, activity = 3, value = 4)

      x_df <- left_join(x_df, mapping, by = "region")
      x_df <- x_df %>%
      mutate(country = ifelse(region == "China", "CHN", country)) %>%
      mutate(country = ifelse(region == "India", "IND", country)) %>%
      filter(!(region == "Non-OECD Asia" & country == "CHN")) %>% # removing duplicates
      filter(!(region == "Non-OECD Asia" & country == "IND")) %>% # removing duplicates
      na.omit() %>%
      select(country, tech, activity, value, -region)

    x <- as.magpie(x_df, spatial = 1, temporal = NULL, datacol = 4)
    getYears(x) <- 2015
    x <- toolCountryFill(x, fill = 0)

   }
  if (subtype == "gas_ef") {
 #   x <- readSource(type = "Rutovitz2015",subtype = "gas_ef",convert = FALSE)
   # getRegions(x) <- gsub("OECD North America","OECD Americas",x = getRegions(x))
   # getRegions(x) <-  gsub("OECD Pacific","OECD Asia Oceania",x = getRegions(x))
   # getRegions(x) <-  gsub("Developing Asia","Non-OECD Asia",x = getRegions(x))
    getItems(x, dim = 1) <- gsub("OECD North America", "OECD Americas", getItems(x, dim = 1))
    getItems(x, dim = 1) <- gsub("OECD Pacific", "OECD Asia Oceania", getItems(x, dim = 1))
    getItems(x, dim = 1) <- gsub("Developing Asia", "Non-OECD Asia", getItems(x, dim = 1))


    mapping$region <- gsub("Central Africa|West Africa|Southern Africa|East Africa|North Africa", "Africa", x = mapping$region)


    x_df <-  as.data.frame(x) %>%
      select(2, 4, 5, 6) %>%
      rename(region = 1, tech = 2, activity = 3, value = 4) %>%
      filter(!region %in% unique(mapping$region))


    x_df$region <- toolCountry2isocode(x_df$region)

    x_df_2 <- as.data.frame(x) %>%
      select(2, 4, 5, 6) %>%
      rename(region = 1, tech = 2, activity = 3, value = 4) %>%
      filter(region %in% unique(mapping$region)) %>%
      left_join(mapping, by = "region") %>%
      filter(!country %in% x_df$region) %>%
      na.omit() %>%
      select(-region) %>%
      rename(region = country) %>%
      select(region, tech, activity, value)

      x_df <- bind_rows(x_df, x_df_2)

      x <- as.magpie(x_df, spatial = 1, temporal = NULL, datacol = 4)
      getYears(x) <- 2015
      x <- toolCountryFill(x, fill = 0)
      # Oil EF for Fuel_supply is same as Gas Fuel supply
      x <- add_columns(x, addnm = "Oil", dim = 3.1)
      x[, , "Oil"] <- x[, , "Gas"]

  }

  return (x)
}
