#' Read JRC IDEES
#' 
#' Read the [IDEES data base from JRC](https://data.jrc.ec.europa.eu/dataset/jrc-10110-10001/resource/f590b6f1-60e5-49a6-a972-60bc2b2e34b3)
#'
#' @md
#' @param subtype one of
#'   - `Chemical Industry`: read the "CHI" worksheets from the IDEES files
#'
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Michaja Pehl
#' 
#' @seealso [`readSource()`]
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows bind_cols select mutate
#' @importFrom readxl read_xlsx
#' @importFrom tidyr drop_na pivot_longer extract
#' @importFrom rlang sym

#' @export
readJRC_IDEES <- function(subtype)
{
  switchboard <- list(
    `Chemical Industry` = function()
    {
      # ---- _ definitions of all the row names to use ----
      # There was no easy way to extract the structure of variables from the 
      # worksheet, since they are differentiated using cell formats (text indent)
      # which is not available through the readxl nor openxlsx packages.
      # 
      # Generating these variable lists is tedious, yet straight-forward:
      # - copy the column of row names from the Excel file to a text file
      # - copy category names in front of variable names, separating them with bars
      #   and removing unneeded units; e.g. 
      #   "Physical output (kt)" + "Basic chemicals (kt ethylene eq.)" becomes
      #   "Physical output|Basic chemicals (kt ethylene eq.)"
      # - empty rows can be left empty, they are ignored during vector concatenation
      # - rows to be ignored (i.e. category names w/o data) are set to NA
      # - matching is strictly positional, no matching what so ever between row 
      #   names and variables is required
      # - turn everything into a data.frame/tibble and include it in the list under
      #   the name of the worksheet; enclosing in curly braces allows for 
      #   code-folding and better overview in RStudio
      variable_mixer <- list(
          'CHI' = {
            tibble(
              name = c(
                'Value added (M\u20ac2010)',
                'Value added|Chemicals and chemical products (M\u20ac2010)',
                'Value added|Chemicals and chemical products|Basic chemicals (M\u20ac2010)',
                'Value added|Chemicals and chemical products|Other chemicals (M\u20ac2010)',
                'Value added|Pharmaceutical products etc. (M\u20ac2010)',
                
                NA,
                'Physical output|Basic chemicals (kt ethylene eq.)',
                'Physical output|Other chemicals (kt ethylene eq.)',
                'Physical output|Pharmaceutical products etc. (kt ethylene eq.)',
                
                NA,
                'Installed capacity|Basic chemicals (kt ethylene eq. production)',
                'Installed capacity|Other chemicals (kt ethylene eq. production)',
                'Installed capacity|Pharmaceutical products etc. (kt ethylene eq. production)',
                NA,
                'Capacity investment|Basic chemicals (kt ethylene eq. production)',
                'Capacity investment|Other chemicals (kt ethylene eq. production)',
                'Capacity investment|Pharmaceutical products etc. (kt ethylene eq. production)',
                NA,
                'Decommissioned capacity|Basic chemicals (kt ethylene eq. production)',
                'Decommissioned capacity|Other chemicals (kt ethylene eq. production)',
                'Decommissioned capacity|Pharmaceutical products etc. (kt ethylene eq. production)',
                NA,
                'Idle capacity|Basic chemicals (kt ethylene eq. production)',
                'Idle capacity|Other chemicals (kt ethylene eq. production)',
                'Idle capacity|Pharmaceutical products etc. (kt ethylene eq. production)',
                
                NA,
                'Energy consumption (ktoe)',
                'Energy consumption|Solids (ktoe)',
                'Energy consumption|Liquids (ktoe)',
                'Energy consumption|Liquids|Refinery gas (ktoe)',
                'Energy consumption|Liquids|LPG (ktoe)',
                'Energy consumption|Liquids|Diesel oil (without biofuels) (ktoe)',
                'Energy consumption|Liquids|Residual fuel oil (ktoe)',
                'Energy consumption|Liquids|Other liquids (ktoe)',
                'Energy consumption|Gas',
                'Energy consumption|Gas|Natural gas (ktoe)',
                'Energy consumption|Gas|Derived gases (ktoe)',
                'Energy consumption|RES and wastes (ktoe)',
                'Energy consumption|RES and wastes|Biomass and wastes (ktoe)',
                'Energy consumption|RES and wastes|Biogas (ktoe)',
                'Energy consumption|RES and wastes|Liquid biofuels (ktoe)',
                'Energy consumption|RES and wastes|Solar (ktoe)',
                'Energy consumption|RES and wastes|Geothermal (ktoe)',
                'Energy consumption|Steam distributed (ktoe)',
                'Energy consumption|Electricity (ktoe)',
                NA,
                'Energy consumption|Basic chemicals (ktoe)',
                'Energy consumption|Other chemicals (ktoe)',
                'Energy consumption|Pharmaceutical products etc. (ktoe)',
                
                'Non-energy use in the Chemical industry (ktoe)',
                NA,
                'Non-energy use|Solids (ktoe)',
                'Non-energy use|Liquids (ktoe)',
                'Non-energy use|Liquids|Refinery gas (ktoe)',
                'Non-energy use|Liquids|LPG (ktoe)',
                'Non-energy use|Liquids|Diesel oil (without biofuels) (ktoe)',
                'Non-energy use|Liquids|Residual fuel oil (ktoe)',
                'Non-energy use|Liquids|Other liquids (ktoe)',
                'Non-energy use|Liquids|Naphtha (ktoe)',
                'Non-energy use|Gas (ktoe)',
                'Non-energy use|Gas|Natural gas (ktoe)',
                'Non-energy use|Gas|Derived gases (ktoe)',
                'Non-energy use|RES and wastes (ktoe)',
                NA,
                'Non-energy use|Basic chemicals (ktoe)',
                'Non-energy use|Other chemicals (ktoe)',
                'Non-energy use|Pharmaceutical products etc. (ktoe)',
                
                'CO2 emissions (kt CO2)',
                'CO2 emissions|energy use related (kt CO2)',
                'CO2 emissions|process emissions (kt CO2)',
                NA,
                'CO2 emissions|Basic chemicals (kt CO2)',
                'CO2 emissions|Other chemicals (kt CO2)',
                'CO2 emissions|Pharmaceutical products etc. (kt CO2)',
                
                NA,
                'Value added intensity|Basic chemicals (VA in \u20ac2010/t of output)',
                'Value added intensity|Other chemicals (VA in \u20ac2010/t of output)',
                'Value added intensity|Pharmaceutical products etc. (VA in \u20ac2010/t of output)',
                NA,
                'Energy intensity|Basic chemicals (toe/t of output)',
                'Energy intensity|Basic chemicals|non energy (toe/t of output)',
                'Energy intensity|Basic chemicals|energy (toe/t of output)',
                'Energy intensity|Other chemicals (toe/t of output)',
                'Energy intensity|Pharmaceutical products etc. (toe/t of output)',
                NA,
                'Useful energy demand intensity|Basic chemicals (toe useful/t of output)',
                'Useful energy demand intensity|Basic chemicals|non energy (toe useful/t of output)',
                'Useful energy demand intensity|Basic chemicals|energy (toe useful/t of output)',
                'Useful energy demand intensity|Other chemicals (toe useful/t of output)',
                'Useful energy demand intensity|Pharmaceutical products etc. (toe useful/t of output)',
                NA,
                'Emission intensity|Basic chemicals (including process emissions) (kt of CO2 / ktoe energy)',
                'Emission intensity|Other chemicals (including process emissions) (kt of CO2 / ktoe energy)',
                'Emission intensity|Pharmaceutical products etc. (kt of CO2 / ktoe energy)'
              )
            ) %>% 
              extract('name', c('variable', 'unit'), '^(.*) \\((.*)\\)$') %>% 
              mutate(!!sym('variable') := paste0('Chemicals Industry|', 
                                                 !!sym('variable')))
          }
        )

      # ---- parsing files ----
      files <- list.files(path = '.',
                          pattern = '^JRC-IDEES-2015_Industry_.*\\.xlsx$')
      
      if (is_empty(files)) {
        stop('Could not find any JRC-IDEES-2015_Industry_.*\\.xlsx files in ', 
             getwd())
      }
          
      tmp <- tibble()
      for (file in files)
      {
        region <- sub('^JRC-IDEES-2015_Industry_(.*)\\.xlsx$', '\\1', file)
        for (sheet in names(variable_mixer))
        {
          tmp <- bind_rows(
            tmp,
            
            read_xlsx(path = file, sheet = sheet) %>%
              # drop empty rows
              drop_na(1) %>%
              # add variable and unit columns
              bind_cols(variable_mixer[[sheet]]) %>%
              # drop unneeded rows
              drop_na('variable', 'unit') %>%
              # drop rownames from worksheet
              select(-1) %>%
              pivot_longer(cols = c(-'variable', -'unit'), names_to = 'year',
                           names_transform = list('year' = as.integer)) %>% 
              mutate(region = region)
          )
        }
      }
      
      tmp %>% 
        select('region', 'year', 'variable', 'unit', 'value') %>% 
        as.magpie(tidy = TRUE) %>% 
        return()
    }
  )
  
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard))))
  {
    stop(paste(
      'Invalid subtype -- supported subtypes are:',
      names(switchboard)
    ))
  }
  else
  {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}