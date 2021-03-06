% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baseValidation.R
\name{getSequenceNumber}
\alias{getSequenceNumber}
\title{Get Sequence Number}
\usage{
getSequenceNumber(timestamps)
}
\arguments{
\item{timestamps}{vector of timestamps}
}
\value{
vector as long as \emph{timestamps} containing numbers 1, 2, 3, ...
  indicating the belonging to a sequence of increasing timestamps. 
  If the vector contains only ones, this means that \emph{timestamps} is 
  increasingly sorted.
}
\description{
Get Sequence Number
}
