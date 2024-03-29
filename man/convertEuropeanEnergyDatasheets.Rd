% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertEuropeanEnergyDatasheets.R
\name{convertEuropeanEnergyDatasheets}
\alias{convertEuropeanEnergyDatasheets}
\title{Convert European Energy Datasheets}
\source{
European Energy Datasheets public database
https://energy.ec.europa.eu/data-and-analysis/eu-energy-statistical-pocketbook-and-country-datasheets_en
}
\usage{
convertEuropeanEnergyDatasheets(x, subtype)
}
\arguments{
\item{x}{European Energy Datasheets magpie object derived from readEuropeanEnergyDatasheets function}

\item{subtype}{data subtype. Either "EU28" (data from June 20 including GBR)
or "EU27" (latest data from August 23 without GBR)}
}
\value{
converted European Energy Datasheets magpie object
}
\description{
Convert European Energy Datasheets
}
\examples{
\dontrun{
test <- readSource("EuropeanEnergyDatasheets", subtype = "EU27", convert = TRUE)
}

}
\author{
Renato Rodrigues and Atreya Shankar
}
