% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baseValidation.R
\name{getOverlappingTimeSequences}
\alias{getOverlappingTimeSequences}
\title{Get Overlapping Time Sequences}
\usage{
getOverlappingTimeSequences(timestamps)
}
\arguments{
\item{timestamps}{vector of date and time objects, inheriting from
\code{POSIXt}}
}
\value{
data frame with columns \emph{sequenceNo}, \emph{tBeg}, \emph{tEnd}.
  In the attribute "sequenceNumber" the vector of sequenceNumbers, each of
  which corresponds to one timestamp is returned.
}
\description{
Get Overlapping Time Sequences
}
