% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readworldsteel.R
\name{readworldsteel}
\alias{readworldsteel}
\title{Read World Steel Statistical Yearbook Data}
\usage{
readworldsteel(subtype = "detailed")
}
\arguments{
\item{subtype}{One of
- `detailed` returning data for the worksheets
  - `Pig Iron Production`
  - `DRI Production`
  - `Total Production of Crude Steel`
  - `Production in Oxygen-Blown Converters`
  - `Production in Open Hearth Furnaces`
  - `Production in Electric Arc Furnaces`
  - `Apparent Steel Use (Crude Steel Equivalent)`
  from 1991 on or
- `long` returning total production data from 1967 on}
}
\value{
A [`magpie`][magclass::magclass] object.
}
\description{
Read combined data of World Steel Association statistical yearbooks
(https://www.worldsteel.org/steel-by-topic/statistics/steel-statistical-yearbook.html).
}
\seealso{
[`readSource()`]
}
\author{
Michaja Pehl
}
