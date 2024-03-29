% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcEmploymentfactors.R
\name{calcEmploymentfactors}
\alias{calcEmploymentfactors}
\title{calcEmploymentfactors}
\usage{
calcEmploymentfactors(improvements, multiplier)
}
\arguments{
\item{improvements}{Either "None", "CEEW", "Dias", "Rutovitz_aus","Solar_found" or "All". Use "All" for all improvements.}

\item{multiplier}{source of regional multiplier. Either "Ram", "Rutovitz", "own" or "static". By default use "own"}
}
\value{
employment factors aggregated by region for certain techs and activities
}
\description{
Collects and combines employment factors for different technologies and activities, from different sources. For all activities except Fuel_supply, units are Full Time Equivalent (FTE)/MW. For Fuel supply, units are FTE/PJ; for nuclear FTE/GWh. The param improvements denotes specific country-level data and is considered an improvement over stock data from rutovitz2015(none)
}
\author{
Aman Malik
}
