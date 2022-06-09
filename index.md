[![R-CMD-check](https://github.com/KWB-R/kwb.base/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.base/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.base/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.base/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.base/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.base)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.base)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.base)](https://kwb-r.r-universe.dev/)

This package originally contained all the
different helper functions developed and used at KWB. As it grew,
functions were extracted into new packages that are dedicated to
certain topics. The database access functions, for example, that were
originally contained here, are now in the package [kwb.db](https://kwb-r.github.io/kwb.db). 
Functions that help to create event information from time series data have been
moved to [kwb.event](https://kwb-r.github.io/kwb.event) and date/time related functions are now in
[kwb.datetime](https://kwb-r.github.io/kwb.datetime). This package still contains some functions that are used
in different scripts of different KWB projects.  Currently, when
loading this package, the packages [kwb.datetime](https://kwb-r.github.io/kwb.datetime), 
[kwb.db](https://kwb-r.github.io/kwb.db), [kwb.event](https://kwb-r.github.io/kwb.event),
[kwb.plot](https://kwb-r.github.io/kwb.plot) and [kwb.utils](https://kwb-r.github.io/kwb.utils) 
are automatically loaded. I plan to change this behaviour with the next release 
so that you need to load the other packages explicitly, i.e. with 
library(kwb.datetime), library(kwb.db), etc.

## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Download and install kwb.base in R
install.packages('kwb.base')

# Browse the kwb.base manual pages
help(package = 'kwb.base')

```
