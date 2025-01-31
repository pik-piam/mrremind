readDLR <- function() {
  countries <- c(
    "Afghanistan",
    "Algeria",
    "Azerbaijan",
    "Albania",
    "Armenia",
    "Andorra",
    "Angola",
    "American Samoa",
    "Argentina",
    "Australia",
    "Austria",
    "Anguilla",
    "Bahrain",
    "Barbados",
    "Botswana",
    "Bermuda",
    "Belgium",
    "Bahamas",
    "Bangladesh",
    "Belize",
    "Bosnia and Herzegovina",
    "Bolivia",
    "Myanmar",
    "Benin",
    "Belarus",
    "Solomon Islands",
    "Brazil",
    "Bhutan",
    "Bulgaria",
    "Bouvet Island",
    "Brunei",
    "Burundi",
    "Canada",
    "Cambodia",
    "Chad",
    "Sri Lanka",
    "Congo",
    "Congo, the Democratic Republic of the",
    "China",
    "China-Disputed",
    "Hong Kong",
    "Macau",
    "Chile",
    "Cameroon",
    "Comoros",
    "Colombia",
    "Costa Rica",
    "Central African Republic",
    "Cuba",
    "Cape Verde",
    "Cyprus",
    "Czech Republic",
    "Denmark",
    "Djibouti",
    "Dominica",
    "Dominican Republic",
    "Ecuador",
    "Egypt",
    "Ireland",
    "Equatorial Guinea",
    "Estonia",
    "Eritrea",
    "El Salvador",
    "Ethiopia",
    "French Guiana",
    "Finland",
    "Fiji",
    "Faroe Islands",
    "French Polynesia",
    "France",
    "Gambia",
    "Gabon",
    "Georgia",
    "Ghana",
    "Gibraltar",
    "Grenada",
    "Guernsey",
    "Germany",
    "Guadeloupe",
    "Greece",
    "Guatemala",
    "Guinea",
    "Guyana",
    "Haiti",
    "Honduras",
    "Croatia",
    "Hungary",
    "Iceland",
    "Indonesia",
    "Isle of Man",
    "India",
    "Iran",
    "Israel",
    "Israel-Disputed (Golan Heights)",
    "West_Bank",
    "Israel-Disputed (Gaza Strip)",
    "Italy",
    "Cote d Ivoire",
    "Iraq",
    "Japan",
    "Jersey",
    "Jamaica",
    "Jordan",
    "Kenya",
    "Kyrgyzstan",
    "North Korea",
    "Kiribati",
    "South Korea",
    "Kuwait",
    "Kazakhstan",
    "Laos",
    "Lebanon",
    "Latvia",
    "Lithuania",
    "Liberia",
    "Slovakia",
    "Liechtenstein",
    "Lesotho",
    "Luxembourg",
    "Libya",
    "Madagascar",
    "Martinique",
    "Moldova",
    "Mongolia",
    "Montserrat",
    "Malawi",
    "FYR of Macedonia",
    "Mali",
    "Monaco",
    "Morocco",
    "Mauritius",
    "Mauritania",
    "Malta",
    "Oman",
    "Maldives",
    "Montenegro",
    "Mexico",
    "Malaysia",
    "Mozambique",
    "New Caledonia",
    "Niger",
    "Vanuatu",
    "Nigeria",
    "Netherlands",
    "Norway",
    "Nepal",
    "Suriname",
    "Netherlands Antilles",
    "Nicaragua",
    "New Zealand",
    "Paraguay",
    "Peru",
    "Pakistan",
    "Poland",
    "Panama",
    "Portugal",
    "Papua New Guinea",
    "Guinea-Bissau",
    "Qatar",
    "Reunion",
    "Romania",
    "Philippines",
    "Puerto Rico",
    "Russia",
    "Rwanda",
    "Saudi Arabia",
    "Saint Kitts and Nevis",
    "Seychelles",
    "South Africa",
    "Senegal",
    "Slovenia",
    "Sierra Leone",
    "San Marino",
    "Singapore",
    "Somalia",
    "Spain",
    "Serbia",
    "Saint Lucia",
    "Sudan",
    "Sweden",
    "Syria",
    "Switzerland",
    "United Arab Emirates",
    "Trinidad and Tobago",
    "Thailand",
    "Tajikistan",
    "Turks and Caicos Islands",
    "Togo",
    "Sao Tome and Principe",
    "Tunisia",
    "Turkey",
    "Taiwan",
    "Turkmenistan",
    "Tanzania",
    "Uganda",
    "United Kingdom",
    "Ukraine",
    "United States",
    "Burkina Faso",
    "Uruguay",
    "Uzbekistan",
    "Saint Vincent and the Grenadines",
    "Venezuela",
    "Virgin Islands, British",
    "Vietnam",
    "Virgin Islands, U.S.",
    "Namibia",
    "Western Sahara",
    "Swaziland",
    "Yemen",
    "Zambia",
    "Zimbabwe"
  )

  iso.countries <- suppressWarnings(toolCountry2isocode(countries))

  ### year
  year <- "y2010"

  ### quality bin
  bins <- utils::read.table("FLh_bins_TriebFormula_2013_11_26.txt", col.names = c("PV", "CSP"))

  ### define which columns to read, psv: odd columns, csp: even columns (exclude first two and last two)
  colClasses.df <- data.frame(PV = character(428), CSP = character(428))
  colClasses.df$PV <- c(rep("NULL", 2), rep(c(NA, "NULL"), length(countries)), rep("NULL", 2))
  colClasses.df$CSP <- c(rep("NULL", 2), rep(c("NULL", NA), length(countries)), rep("NULL", 2))

  ### csv files
  csv.files <- list.files(pattern = ".csv$")

  ### read csv function
  read.csv.Solar <- function(x) {
    technology <- ifelse(grepl(x = x, pattern = "PV"), "PV",
      ifelse(grepl(x = x, pattern = "CSP"), "CSP", NA)
    )

    distance <- ifelse(grepl(x = x, pattern = "1-50"), "0-50",
      ifelse(grepl(x = x, pattern = "1-100"), "0-100",
        ifelse(grepl(x = x, pattern = "1-inf"), "0-inf", NA)
      )
    )

    type <- ifelse(grepl(x = x, pattern = "_MW_"), "capacity",
      ifelse(grepl(x = x, pattern = "_area_"), "area", NA)
    )

    bin <- bins[, technology]

    out <- utils::read.csv2(x, header = FALSE, colClasses = colClasses.df[, technology])
    colnames(out) <- iso.countries
    out$type <- type
    out$technology <- technology
    out$distance <- distance
    out$bin <- bin

    ### add empty data frames for csp area data

    if (all(technology == "CSP", type == "capacity")) {
      out.area.csp <- out
      out.area.csp$type <- "area"
      out.area.csp[, 1:length(iso.countries)] <- NA

      out <- rbind(out, out.area.csp)
    }

    return(out)
  }


  ### read data

  df <- do.call(rbind, lapply(csv.files, read.csv.Solar))


  dat <- reshape2::melt(df, id.vars = c("technology", "distance", "bin", "type"))
  # sort bin dimension
  dat <- dat[order(dat$bin), ]

  dat$Year <- year
  dat <- dat[c(5, 7, 4, 1, 2, 3, 6)]
  colnames(dat) <- c("region", "Year", "Type", "Technology", "Distance", "Bin", "Value")
  dat$region <- as.character(dat$region)
  dat <- dat[!(dat$region == "ANT"), ]

  dat <- dat[which(!is.na(dat$region)), ]

  out <- as.magpie(dat, datacol = 7)

  return(out)
}
