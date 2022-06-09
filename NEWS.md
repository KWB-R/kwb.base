# [kwb.base 0.5.0](https://github.com/KWB-R/kwb.base/releases/tag/v0.5.0)  <small>2022-06-09</small>

* Harmonise with R package [kwb.pkgbuild](https://github.com/kwb-r/kwb.pkgbuild),
e.g. use GitHub actions as CI (instead of travis/appeyor)

* Added a `NEWS.md` file to track changes to the package.

* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`


# [kwb.base 0.4.0](https://github.com/KWB-R/kwb.base/releases/tag/v0.4.0)  <small>2018-06-15</small>

* Convert `inlinedocs` comments to Roxygen comments. Move to 
https://github.com/kwb-r/kwb.base with this version (*all former versions were under 
version control SVN within the KWB intranet!*)

# kwb.base 0.3.0

* `hsFileLines` removed (simply use `readLines`)
* `hsPrepPdf`, `hsShowPdf`, `hsMatrixToListForm` moved to [kwb.utils](https://github.com/kwb-r/kwb.utils)

# kwb.base 0.2.4

* package now depends on kwb.utils, kwb.db
* `hsQuoteChr`, `hsSafeName`, `hsToPosix` moved to [kwb.utils](https://github.com/kwb-r/kwb.utils)
* all database related functions now in [kwb.db](https://github.com/kwb-r/kwb.db)

# kwb.base 0.2.3

* new functions: `hsFilterRowsWithValuesInColumns`, `hsSetNamedMatrixColumnsToValues`, 
  `hsRenameColumns`

# kwb.base 0.2.2

* new functions: `hsDumpMdb`, `hsSetForeignKey`, `hsMovingMean`
* `hsJoinEvents`: can now be used with events produced by `hsEventsOnChange`

# kwb.base 0.2.1

* new function: `hsSubstSpecChars`

# kwb.base 0.2.0

* preliminary version to be distributed to MIA-CSO partners

# kwb.base 0.1.6

* `xmdb`: now returning path to example-mdb in package's sub-folder "extdata"
* `hsEvents`: if data frame is given, first column is assumed to contain timestamps
    new arg "evtSepOp"
* `hsGetEvent`: new arg 'useIndex'
* `hsGroupByInterval`: new arg '...'
* New functions `hsNiceLabels`, `hsDateStr`, `hsDaylightSaving`, `hsST2WT`

# kwb.base 0.1.5

* `hsFields`: bug fix. Now supporting table names with spaces/special characters.
* New functions `hsGetEvent`, `hsGroupByInterval`

# kwb.base 0.1.4

* `hsSigWidth`: New arg 'dbg', respection of time unit
* `hsMergeEvents`, `hsJoinEvents`: respection of optional event pauses and time units
* `hsEvents`: New args 'pause', 'tUnit'
* New functions `hsAddMissingCols`, `hsDelEmptyCols`, `hsEventsToUnit`

# kwb.base 0.1.3

* `hsShowPdf`: enabling paths containing spaces
* `hsMdbTimeSeries`, `hsGetTimeSeries`: New arg 'inclLast'
* New functions `hsCheckForCols`, `hsSigWidth`, `hsMergeEvents`, `hsJoinEvents`

# kwb.base 0.1.2

* `hsNearestStepMult`: New arg 'direction' allows for "rounding" timestamps to 
nearest, nearest smaller, or nearest greater time step multiple. 
All callers changed.
* New functions `hsFileLines`, `hsShowPdf`