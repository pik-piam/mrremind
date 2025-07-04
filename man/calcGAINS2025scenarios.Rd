% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcGAINS2025scenarios.R
\name{calcGAINS2025scenarios}
\alias{calcGAINS2025scenarios}
\title{Calculates air pollutant emissions, activities and emission factors
for all scenarios and SSPs available from GAINS, at the level
of GAINS sectors.}
\usage{
calcGAINS2025scenarios(subtype, agglevel = "agg")
}
\arguments{
\item{subtype}{"emission_factors", "emissions","emissions_starting_values"}

\item{agglevel}{"agg" or "det", sectoral aggregation level}
}
\value{
Activity levels, emissions or emission factors
}
\description{
This function is meant to be used to prepare the GAINS data
in the most inclusive format possible. The actual generation
of REMIND-specific files happens elsewhere, and uses this function.
}
\details{
Extrapolates activities and emissions for the 2050-2100 periods
assuming a relationship between changes in polluting activities
and GDP.
}
\author{
Gabriel Abrahao
}
