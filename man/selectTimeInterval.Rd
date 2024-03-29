% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hsLibDateTime.R
\name{selectTimeInterval}
\alias{selectTimeInterval}
\title{Select Time Interval}
\usage{
selectTimeInterval(
  x,
  t1 = NULL,
  t2 = NULL,
  width = "-7d",
  posixColumn = kwb.utils::posixColumnAtPosition(x),
  dbg = TRUE
)
}
\arguments{
\item{x}{data frame with at least one time column}

\item{t1}{first timestamp as text, in yyyy-mm-dd format}

\item{t2}{optional. last timestamp as text, in yyyy-mm-dd format}

\item{width}{interval width as text containing number and unit (one of "d" -
day, "h" - hour, "m" - minute). E.g. "7d" = 7 days, "20m" = 20 minutes. The
number may be negative indicating "last <number> units".}

\item{posixColumn}{name or number of column in \code{x} containing the
relevant timestamps}

\item{dbg}{if \code{TRUE}, debug messages are shown}
}
\description{
Select Time Interval
}
