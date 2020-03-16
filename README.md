# R moinput package

## Purpose and Functionality

The R-library moinput provides useful functions and a common structure to all the input data required to run models like MAgPIE and REMIND


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("moinput")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r 
vignette("moinput")
```

## Travis CI Integration

[![Travis build status](https://travis-ci.com/pik-piam/moinput.svg?branch=master)](https://travis-ci.com/pik-piam/moinput)


## Questions / Problems

In case of questions / problems please contact Jan Dietrich <dietrich@pik-potsdam.de>.

## Citation

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3699594.svg)](https://doi.org/10.5281/zenodo.3699594)
