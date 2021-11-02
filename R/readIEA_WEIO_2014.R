#' Title
#'
#' @return
#' 
#' @importFrom quitte list_to_data_frame_
#' @importFrom readxl read_xls

#' @export
readIEA_WEIO_2014 <- function() {
  
  # define country groups ----
  
  ## IEA groups ----
  # IEA regions taken from "IEA World Energy Outlook Special Report 2015"
  country_groups <- list(
    OECDAMS = c('Canada', 'Chile', 'Mexico', 'United States'),
    US = 'United States',
    OECDEUR = c('Austria', 'Belgium', 'Czech Republic', 'Denmark', 'Estonia', 
                'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland',
                'Ireland', 'Italy', 'Luxembourg', 'Netherlands', 'Norway',
                'Poland', 'Portugal', 'Slovak Republic', 'Slovenia', 'Spain', 
                'Sweden', 'Switzerland', 'Turkey', 'United Kingdom', 'Israel'),
    EU28 = c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 
             'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 
             'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy', 'Latvia', 
             'Lithuania', 'Luxembourg', 'Malta', 'Netherlands', 'Poland', 
             'Portugal', 'Romania', 'Slovak Republic', 'Slovenia', 'Spain', 
             'Sweden', 'United Kingdom'),
    OECDAOC = c('Australia', 'Japan', 'Korea', 'New Zealand'),
    JPN = 'Japan',
    E.Europe = c('Albania', 'Armenia', 'Azerbaijan', 'Belarus', 
                 'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 'Georgia', 
                 'Kazakhstan', 
                 # 'Kosovo', FIXME
                 'Kyrgyz Republic', 'Latvia', 'Lithuania', 
                 'former Yugoslav Republic of Macedonia', 'Republic of Moldova',
                 'Montenegro', 'Romania', 'Russian Federation', 'Serbia', 
                 'Tajikistan', 'Turkmenistan', 'Ukraine', 'Uzbekistan', 
                 'Cyprus', 'Gibraltar', 'Malta'),
    RUS = 'Russian Federation',
    NonOECDAsia = c('Bangladesh', 'Brunei Darussalam', 'Cambodia', 'China', 
                    'Hong Kong', 'Chinese Taipei', 'India', 'Indonesia', 
                    'Democratic People’s Republic of Korea', 'Malaysia', 
                    'Mongolia', 'Myanmar', 'Nepal', 'Pakistan', 'Philippines', 
                    'Singapore', 'Sri Lanka', 'Thailand', 'Vietnam',
                    'Afghanistan', 'Bhutan', 'Cook Islands', 'East Timor',
                    'Fiji', 'French Polynesia', 'Kiribati', 'Lao PDR', 
                    'Macao (China)', 'Maldives', 'New Caledonia', 'Palau', 
                    'Papua New Guinea', 'Samoa', 'Solomon Islands', 'Tonga',
                    'Vanuatu'),
    China = c('China', 'Hong Kong'),
    India = 'India',
    ASEAN = c('Brunei Darussalam', 'Cambodia', 'Indonesia', 'Lao PDR', 
              'Malaysia', 'Myanmar', 'Philippines', 'Singapore', 'Thailand',
              'Vietnam'),
    Africa = c('Algeria', 'Egypt', 'Libya', 'Morocco', 'Tunisia', 
               'Western Sahara',
               'Angola', 'Benin', 'Botswana', 'Burkina Faso', 'Burundi', 
               'Cabo Verde', 'Cameroon', 'Comoros', 
               'Central African Republic', 'Chad', 'Congo', 'Côte d’Ivoire', 
               'Democratic Republic of Congo', 'Djibouti', 'Equatorial Guinea', 
               'Eritrea', 'Ethiopia', 'Gabon', 'Gambia', 'Ghana', 'Guinea', 
               'Guinea-Bissau', 'Kenya', 'Lesotho', 'Liberia', 'Madagascar', 
               'Malawi', 'Mali', 'Mauritania', 'Mauritius', 'Mozambique', 
               'Namibia', 'Niger', 'Nigeria', 'Rwanda', 'Sao Tome and Principe', 
               'Senegal', 'Seychelles', 'Sierra Leone', 'South Africa', 
               'Somalia', 'South Sudan', 'Sudan', 'Swaziland', 'Togo', 'Uganda', 
               'United Republic of Tanzania', 'Zambia', 'Zimbabwe'),
    LAM = c('Argentina', 'Bolivia', 'Brazil', 'Colombia', 'Costa Rica', 'Cuba', 
            'Dominican Republic', 'Ecuador', 'El Salvador', 'Guatemala', 
            'Haiti', 'Honduras', 'Jamaica', 
            # Netherlands Antilles
            'Bonaire, Saba, Sint Eustatius', 'Aruba', 'Sint Maarten', 'Curaçao',
            'Nicaragua', 'Panama', 'Paraguay', 'Peru', 'Trinidad and Tobago', 
            'Uruguay',  'Venezuela', 'Antigua and Barbuda', 'Aruba', 'Bahamas', 
            'Barbados', 'Belize',  'Bermuda', 'British Virgin Islands', 
            'Cayman Islands', 'Dominica',  'Falkland Islands (Malvinas)', 
            'French Guyana', 'Grenada',  'Guadeloupe', 'Guyana', 'Martinique', 
            'Montserrat',  'St. Kitts and Nevis', 'St Lucia', 
            'St Pierre et Miquelon', 'St. Vincent and the Grenadines', 
            'Suriname', 'Turks and Caicos Islands'),
    Brazil = 'Brazil',
    ME = c('Bahrain', 'Islamic Republic of Iran', 'Iraq', 'Jordan', 'Kuwait', 
           'Lebanon', 'Oman', 'Qatar', 'Saudi Arabia', 'Syrian Arab Republic',
           'United Arab Emirates', 'Yemen')
  )
  
  ## combine OECD and World ----
  country_groups <- c(
    country_groups,
    
    list(
      # combine OECD countries
      OECD = unlist(country_groups[c('OECDAMS', 'OECDEUR', 'OECDAOC')], 
                    use.names = FALSE),
      # combine World countries
      World = unique(unlist(country_groups, use.names = FALSE))
    )
  )
  
  ## define additional country groups ----
  additional_country_groups <- tribble(
    ~country.group,                       ~sub.group,        ~weight,
    # OECD Americas w/o USA
    'OECDAMS_wo_US',                      'OECDAMS',          1,
    'OECDAMS_wo_US',                      'US',              -1,
    
    # OECD Asia Oceania w/o Japan
    'OECDAOC_wo_JPN',                     'OECDAOC',          1,
    'OECDAOC_wo_JPN',                     'JPN',             -1,
    
    # Easter Europe/Eurasia w/o Russian Federation
    'E.Europe_wo_RUS',                    'E.Europe',         1,
    'E.Europe_wo_RUS',                    'RUS',             -1,
    
    # Non-OECD w/o China, India, and ASEAN
    'NonOECDAsia_wo_China_India_ASEAN',   'NonOECDAsia',      1,
    'NonOECDAsia_wo_China_India_ASEAN',   'China',           -1,
    'NonOECDAsia_wo_China_India_ASEAN',   'India',           -1,
    'NonOECDAsia_wo_China_India_ASEAN',   'ASEAN',           -1,
    
    # Latin America w/o Brazil
    'LAM_wo_Brazil',                      'LAM',              1,
    'LAM_wo_Brazil',                      'Brazil',          -1,
    
    # OECD Europe and EU28
    'OECDEUR_and_EU28',                   'OECDEUR',          1,
    'OECDEUR_and_EU28',                   'EU28',             1,
    
    # OECD Europe w/o EU28
    'OECDEUR_cut_EU28',                   'OECDEUR',          1,
    'OECDEUR_cut_EU28',                   'EU28',            -1,
    
    # EU28 w/o OECD Europe
    'EU28_cut_OECDEUR',                   'OECDEUR',         -1,
    'EU28_cut_OECDEUR',                   'EU28',             1)
  
  # read data ----
  d <- lapply(names(country_groups), function(sheet) {
    read_xls(path = '/home/pehl/PIK/Zotero/storage/2YN7XHKU/WEIO2014AnnexA.xls',
             sheet = sheet,
             range = 'C39:F40',
             col_names = c('sector', 'value'),
             col_types = c('text', 'skip', 'skip', 'numeric')) %>% 
      mutate(country.group = sheet)
  }) %>% 
    bind_rows()
  
  # expand country groups to countries ----
  list_to_data_frame_(country_groups, 'country.group', 'country.name') %>% 
    add_countrycode_(origin = 'country.name', destination = 'iso3c')
  

  
  
  
  additional_country_groups_iso3c <- full_join(
    additional_country_groups,
    
    list_to_data_frame_(country_groups, 'country.group', 'country.name') %>% 
      add_countrycode_(origin = 'country.name', destination = 'iso3c') %>% 
      select(-'country.name'),
    
    c('sub.group' = 'country.group')
  ) %>% 
    group_by(.data$country.group, .data$iso3c) %>% 
    summarise(weight = pmin(1, sum(.data$weight)), .groups = 'drop') %>% 
    filter(0 < .data$weight)
}
