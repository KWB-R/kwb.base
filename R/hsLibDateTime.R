# Test hsDaylightSaving (vs kwb.datetime::date_range_CEST)
if (FALSE)
{
  min_year <- 1980
  max_year <- 2200
  
  system.time(x <- kwb.base::hsDaylightSaving(min_year, max_year))
  system.time(y <- hsDaylightSaving(min_year, max_year))
  
  system.time(z <- kwb.datetime::date_range_CEST(min_year:max_year))
  
  all(x == y)
  
  do.call(rbind, lapply(years, date_range_CEST))
  
  x$begST_2 <- sapply(years, begin_of_CEST)
  
  x
  
  all(as.character(x$endST) == x$endST_2)
  all(as.character(x$begST) == x$begST_2)
}

# dataFrameToXts ---------------------------------------------------------------

#' Convert Time Series Data Frame to xts Object
#'
#' @param dataFrame data frame containing a timestamp column
#' @param timeColumn name or number of time column. Default: first POSIXt-column
#'   in \emph{dataFrame}
#'
#' @return xts object with timestamps taken from timestamp column. Non-numeric
#'   columns are removed.
#' @importFrom xts xts
dataFrameToXts <- function(
  dataFrame, timeColumn = names(dataFrame)[kwb.utils::posixColumnAtPosition(dataFrame)[1]]
)
{ 
  numericColumns <- sapply(
    names(dataFrame), FUN = function(x) is.numeric(dataFrame[[x]])
  )
  
  xts::xts(
    x = dataFrame[, numericColumns, drop = FALSE], 
    order.by = dataFrame[[timeColumn]],
    tzone = attr(dataFrame[[timeColumn]], which = "tzone")
  )
}

# selectTimeIntervalDays -------------------------------------------------------

#' Select Time Interval Days
#' 
#' @param dat data frame with column "DateTime"
#' @param days number of "last" days to select
#' @param firstDay first day as text, in "yyyy-mm-dd" format
#' @param lastDay last day as text, in "yyyy-mm-dd" format  
#' @param dbg if \code{TRUE}, debug messages are shown
#' @importFrom kwb.utils catIf
#' 
selectTimeIntervalDays <- function(
  dat, days = 7, firstDay = as.character(as.Date(lastDay) - days),
  lastDay = substr(utils::tail(dat$DateTime, 1), 1, 10), dbg = TRUE
) 
{  
  kwb.utils::catIf(dbg, sprintf(
    "days: %d\nfirstDay: %s\nlastDay:  %s\n", days, firstDay, lastDay
  ))

  dates <- as.Date(dat$DateTime)
  included <- which((dates >= as.Date(firstDay)) & (dates <= as.Date(lastDay)))
  
  x <- dat[included, ]
  
  if (!isNullOrEmpty(included)) {
    
    attr(x, "rowRange") <- range(included)
  }
  
  x
}

# selectTimeInterval -----------------------------------------------------------

#' Select Time Interval
#'
#' @param x data frame with at least one time column
#' @param t1 first timestamp as text, in yyyy-mm-dd format
#' @param t2 optional. last timestamp as text, in yyyy-mm-dd format
#' @param width interval width as text containing number and unit (one of "d" -
#'   day, "h" - hour, "m" - minute). E.g. "7d" = 7 days, "20m" = 20 minutes. The
#'   number may be negative indicating "last <number> units".
#' @param posixColumn name or number of column in \code{x} containing the
#'   relevant timestamps
#' @param dbg if \code{TRUE}, debug messages are shown
#' @importFrom kwb.utils posixColumnAtPosition
selectTimeInterval <- function(x, t1 = NULL, t2 = NULL, width = "-7d",
  posixColumn = kwb.utils::posixColumnAtPosition(x), dbg = TRUE
)
{
  seconds <- intervalWidthToSeconds(width)
  
  firstTimestamp <- x[[posixColumn]][1]
  lastTimestamp <- x[[posixColumn]][nrow(x)]
  
  if (is.null(t1) && is.null(t2)) {
    
    if (seconds < 0) {
      
      t2 <- lastTimestamp
      t1 <- lastTimestamp + seconds
      
    } else {
      
      t1 <- firstTimestamp
      t2 <- firstTimestamp + seconds
    }
    
  } else if (is.null(t1)) {
    
    t1 <- kwb.datetime::hsToPosix(t2) + seconds
    
  } else if (is.null(t2)) {
    
    t2 <- kwb.datetime::hsToPosix(t1) + seconds
  }
  
  kwb.utils::catIf(dbg, sprintf(
    "\n*** selectTimeInterval: t1=%s, t2=%s, width=%s\n", t1, t2, width
  ))

  x[timestampIn(x[[posixColumn]], t1, t2), ]
}

# intervalWidthToSeconds -------------------------------------------------------

#' Interval Width to Seconds
#' 
#' @param intervalWidth character string starting with numeric characters 0-9
#'   and ending with either of "d" (days), "h" (hours) or "m" (minutes)
#' 
intervalWidthToSeconds <- function(intervalWidth)
{
  secondsPerUnit <- c(d = 86400, h = 3600, m = 60)
  
  n <- nchar(intervalWidth)
  
  widthValue <- as.numeric(substr(intervalWidth, 1, n - 1))
  
  widthUnit <- substr(intervalWidth, n, n)
  
  as.integer(widthValue * secondsPerUnit[widthUnit])
}

# firstTimestamp ---------------------------------------------------------------

#' First Timestamp in Data Frame
#' 
#' @param x data frame containing a date/time column
#' @importFrom utils head
firstTimestamp <- function(x)
{
  timestamps <- firstPosixColumn(x)
  
  firstTs <- utils::head(timestamps, 1)
  
  if (firstTs != min(timestamps)) {
    
    warning("The first timestamp is not the smallest timestamp!")
  }
  
  firstTs
}

# lastTimestamp ----------------------------------------------------------------

#' Last Timestamp in Data Frame
#' 
#' @param x data frame containing a date/time column  
#' @importFrom utils tail
lastTimestamp <- function(x)
{
  timestamps <- firstPosixColumn(x)
  
  lastTs <- utils::tail(timestamps, 1)
  
  if (lastTs != max(timestamps)) {
    
    warning("The last timestamp is not the biggest timestamp!")
  }  
  
  lastTs
}

# hsST2WT ----------------------------------------------------------------------

#' Convert Summer Time to Winter Time
#' 
#' Conversion of time series in summer time to time series in winter time
#' 
#' @param tstamps timestamps in summer time
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
#' @return time series, shifted to winter time (timezone is set to "UTC")
#' @importFrom kwb.utils printIf
hsST2WT <- function(tstamps, dbg = FALSE)
{
  ## "hardcode" timezone to UTC, thus taking timestamps as they are and 
  ## preventing from any time conversion
  tstamps.st <- kwb.datetime::hsToPosix(format(tstamps, "%Y-%m-%d %H:%M:%S"))
  n <- length(tstamps.st)
  
  ## Which years are contained in tstamps?
  years <- sort(unique(as.integer(format(tstamps.st, "%Y"))))
  
  ## Find days of switch to/from daylight saving time
  yearRange <- range(years)
  dss <- hsDaylightSaving(year.first = yearRange[1], year.last = yearRange[2])
  
  kwb.utils::printIf(dbg, dss, "begin and end of daylight saving time in included years")

  ## Init result timestamps with timestamps in summer time
  tstamps.wt <- tstamps.st
  
  ## Loop through different years
  for (i in seq_len(nrow(dss))) {
    
    ## Limits of summer/winter time
    tBegST3h <- kwb.datetime::hsToPosix(paste(dss$begST[i], "03:00:00"))
    tEndST2h <- kwb.datetime::hsToPosix(paste(dss$endST[i], "02:00:00"))
    tEndST3h <- kwb.datetime::hsToPosix(paste(dss$endST[i], "03:00:00"))
    
    catIf(dbg, sprintf(
      "i:%d\ntBegST3h: %s\ntEndST2h: %s\ntEndST3h: %s\n", 
      i, tBegST3h, tEndST2h, tEndST3h
    ))
    
    ## Find indices of timestamps that lie within summer time interval    
    idx.st  <- which(tstamps.st >= tBegST3h & tstamps.st < tEndST2h)
    idx.amb <- which(tstamps.st >= tEndST2h & tstamps.st < tEndST3h)
    
    kwb.utils::printIf(dbg, tstamps.st[idx.amb], "Ambiguous timestamps")      

    ## Shift timestamps that are certainly in summer time back to winter time
    tstamps.wt[idx.st] <- tstamps.st[idx.st] - 3600              
    
    ## number of ambiguous timestamps
    namb <- length(idx.amb)
    
    ## If there are ambiguous timestamps, try to convert them to winter time...
    if (namb > 0) {
      
      ## number of ambiguous timestamps must be even      
      if (namb %% 2 != 0) {
        msg <- paste("Number of ambiguous timestamps is not even as expected!",
                     "Ambiguous timestamps are:", 
                     paste(tstamps.st[idx.amb], collapse = "\n"))
        stop(msg)
      }
      
      ## 1st and 2nd half of the ambiguous timestamps must be identical
      amb.1st <- tstamps.st[idx.amb[1]:idx.amb[namb/2]]
      amb.2nd <- tstamps.st[idx.amb[namb/2+1]:idx.amb[namb]]
      
      if (! all(amb.1st == amb.2nd)) {
        
        kwb.utils::printIf(dbg, amb.1st, "1st half of ambiguous timestamps")
        kwb.utils::printIf(dbg, amb.2nd, "2nd half of ambiguous timestamps")
          
        stop("Cannot convert ambiguous timestamps!")      
        
      } else {
        
        ## The first half of ambiguous timestamps needs to be shifted back by one
        ## hour to winter time whereas the second half already is in winter time
        tstamps.wt[idx.amb] <- c(amb.2nd - 3600, amb.2nd)
      }          
    }
  }
  
  tstamps.wt
}

# hsDaylightSaving -------------------------------------------------------------

#' Find Days of Daylight Saving
#'
#' @param year.first first year of which daylight saving dates are to be
#'   calculated
#' @param year.last last year of which daylight saving dates are to be
#'   calculated
#'
#' @return data frame with columns \code{begST} (begin of summer time) and
#'   \code{endST} (end of summer time)
#' @importFrom  kwb.utils defaultIfNULL
#' @importFrom  kwb.datetime hsDateStr
hsDaylightSaving <- function(year.first = NULL, year.last = NULL)
{ 
  thisYear <- as.integer(format(Sys.Date(), "%Y"))
  
  year.first <- kwb.utils::defaultIfNULL(year.first, thisYear - 5)
  year.last  <- kwb.utils::defaultIfNULL(year.last,  thisYear + 5)

  t0 <- as.POSIXlt(sprintf("%d-01-01", year.first), tz = "MET")
  t1 <- as.POSIXlt(sprintf("%d-12-31", year.last), tz = "MET")                 
  
  ## Daily timestamps
  timestamps <- seq(t0, t1, by = 24 * 3600)
  
  ## Hour of daily timestamps
  hours <- format(timestamps, "%H")
  n_hours <- length(hours)
  
  ## Get indices of hours after which the hour changes between 00 and 01
  ## Compare kwb.utils::hsEventsOnChange!
  indices <- c(which(hours[-n_hours] != hours[-1]), n_hours)

  ## Get "switch" dates
  switch_dates <- kwb.datetime::hsDateStr(timestamps[indices])
  
  n <- length(switch_dates)
  
  if (n != 2 * (year.last - year.first + 1) + 1) {
    
    stop("Sorry, I cannot determine the periods of daylight savings time for ", 
         "the years between ", year.first, " and ", year.last, call. = FALSE)
  }
  
  ## Return result
  data.frame(
    begST = switch_dates[seq(1, n - 1, 2)], 
    endST = switch_dates[seq(2, n, 2)]
  )
}

# hsMkTimestamps ---------------------------------------------------------------

#' Sequence of Timestamps
#'
#' Creates timestamps between first timestamp \emph{from} and \emph{to} with a
#' distance of \emph{step.s} seconds between the timestamps. If \emph{mdb} is
#' given, the timestamps are written to a database table in which the timestamp
#' field is the primary key.
#'
#' @param from first timestamp in ISO-Syntax: yyyy-mm-dd [HH:MM:SS] where the
#'   part in brackets is optional.
#' @param to last timestamp in ISO-Syntax: yyyy-mm-dd [HH:MM:SS] where the part
#'   in brackets is optional.
#' @param step.s time step between the timestamps in seconds.
#' @param mdb Optional. Full path to MS Access database file (*.mdb).
#' @param tbl Optional. Name of table to be created in \emph{mdb}. If no name is
#'   given a name of the type tbl\emph{from}_to_\emph{to}_\emph{step.s}_s is
#'   created. If a table of given name exists, a non-existing name is generated
#'   first, so existing tables will not be overwritten.
#' @param dbg if TRUE, debug messages are shown.
#'
#' @return Returns vecotor of timestamps if \emph{mdb} is missing or nothing if
#'   timestamp table has been generated in database given in \emph{mdb}.
#'
#' @examples
#' \dontrun{
#' ## Write timestamps of January 2011 with five minutes step into example db.
#' hsMkTimestamps("2011-01-01", "2011-02-01", 300, kwb.db::xmdb())
#' }
#' 
#' ## Output:
#' # Timestamps have been written to table
#' #   'tblTimestamps_2011_01_01_to_2011_02_01_300s' in
#' # 'C:/Users/hsonne/Documents/R/win-library/2.14/
#' #   kwb.base/extdata/RExKwbBase.mdb'.
#' # Timestamp field has been set as primary key.
#' @importFrom kwb.datetime hsToPosix
#' @importFrom kwb.db hsSetPrimaryKey hsPutTable
hsMkTimestamps <- function(from, to, step.s = 60, mdb, tbl, dbg = FALSE)
{
  # Create sequence of POSIXct timestamps in UTC
  tstamps = seq(kwb.datetime::hsToPosix(from), kwb.datetime::hsToPosix(to), by = step.s)

  # Return vector of timestamps if no database is given
  if (missing(mdb)) return(tstamps)
  
  # Create table name if no table name is given
  strFormat <- "%Y_%m_%d"
  if (missing(tbl)) tbl <- sprintf("tblTimestamps_%s_to_%s_%ds", 
    format.Date(from, strFormat), format.Date(to, strFormat), step.s)

  # Save table in database
  tbl <- kwb.db::hsPutTable(mdb, data.frame(tstamp = tstamps), tbl, dbg = dbg)
  if (length(tbl) > 0) 
    cat(sprintf("Timestamps have been written to table '%s' in '%s'.\n", 
      tbl, mdb))
  
  # Set field "tstamps" as primary key
  res <- kwb.db::hsSetPrimaryKey(mdb, tbl, "tstamp", dbg = dbg)
  cat("Timestamp field has been set as primary key.\n")
}
