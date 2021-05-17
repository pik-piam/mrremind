# MadRat REMIND Input Data Package

R package **mrremind**, version **0.60.3**

[![CRAN status](https://www.r-pkg.org/badges/version/mrremind)](https://cran.r-project.org/package=mrremind)   [![R build status](https://github.com/pik-piam/mrremind/workflows/check/badge.svg)](https://github.com/pik-piam/mrremind/actions) [![codecov](https://codecov.io/gh/pik-piam/mrremind/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/mrremind)

## Purpose and Functionality

The mrremind packages contains data preprocessing for the REMIND model.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrremind")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Lavinia Baumstark <lavinia@pik-potsdam.de>.

## Citation

To cite package **mrremind** in publications use:

Baumstark L, Rodrigues R, Levesque A, Oeser J, Bertram C, Mouratiadou I, Malik
A, Schreyer F, Soergel B, Rottoli M, Mishra A, Dirnaichner A, Pehl M,
Giannousakis A, Klein D, Strefler J, Feldhaus L, Brecha R, Rauner S, Dietrich
J, Bi S, Benke F (2021). _mrremind: MadRat REMIND Input Data Package_. R
package version 0.60.3.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrremind: MadRat REMIND Input Data Package},
  author = {Lavinia Baumstark and Renato Rodrigues and Antoine Levesque and Julian Oeser and Christoph Bertram and Ioanna Mouratiadou and Aman Malik and Felix Schreyer and Bjoern Soergel and Marianna Rottoli and Abhijeet Mishra and Alois Dirnaichner and Michaja Pehl and Anastasis Giannousakis and David Klein and Jessica Strefler and Lukas Feldhaus and Regina Brecha and Sebastian Rauner and Jan Philipp Dietrich and Stephen Bi and Falk Benke},
  year = {2021},
  note = {R package version 0.60.3},
}
```

