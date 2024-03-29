% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readDias.R
\name{readDias}
\alias{readDias}
\title{readDias
Contains employment factors and direct jobs for coal power, employment factors and indirect jobs
for coal mining for EU countries. Numbers taken from Dias et al. (2018)}
\usage{
readDias(subtype)
}
\arguments{
\item{subtype}{Employment factors or Employment}
}
\value{
magpie object containing either employment or employment factors.
}
\description{
readDias
Contains employment factors and direct jobs for coal power, employment factors and indirect jobs
for coal mining for EU countries. Numbers taken from Dias et al. (2018)
}
\examples{
 \dontrun{ a <- readSource(type="Dias",convert=FALSE) }
}
\author{
Aman Malik
}
