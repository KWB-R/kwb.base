[![R-CMD-check](https://github.com/KWB-R/kwb.base/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.base/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.base/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.base/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.base/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.base)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.base)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.base)](https://kwb-r.r-universe.dev/)

# kwb.base

This package originally contained all the
different helper functions developed and used at KWB. As it grew,
functions were extracted into new packages that are dedicated to
certain topics. The database access functions, for example, that were
originally contained here, are now in the package kwb.db. Functions
that help to create event information from time series data have been
moved to kwb.event and date/time related functions are now in
kwb.datetime. This package still contains some functions that are used
in different scripts of different KWB projects.  Currently, when
loading this package, the packages kwb.datetime, kwb.db, kwb.event,
kwb.plot and kwb.utils are automaticallc loaded. I plan to change this
behaviour with the next release so that you need to load the other
packages explicitly, i.e. with library(kwb.datetime), library(kwb.db),
etc.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.base' from GitHub
remotes::install_github("KWB-R/kwb.base")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.base](https://kwb-r.github.io/kwb.base)

Development: [https://kwb-r.github.io/kwb.base/dev](https://kwb-r.github.io/kwb.base/dev)
