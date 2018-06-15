# call: checkForOverlappingTimeSequences(hydraulicData$DateTime)

# checkForOverlappingTimeSequences ---------------------------------------------

#' Check for Overlapping Time Sequences
#' 
#' @param dataFrame frame with timestamps in first column
#' @param main plot title, passed to \code{\link{plotSequenceEvents}}
#' 
#' @return \code{TRUE} if there are overlapping sequences, else \code{FALSE}
#' 
checkForOverlappingTimeSequences <- function(
  dataFrame, main = "Overlapping time sequences in hydraulic data"
)
{
  timestamps <- dataFrame[[1]]
  
  stopifnot(inherits(timestamps, "POSIXt"))
  
  sequences <- getOverlappingTimeSequences(timestamps)
  
  if (nrow(sequences) > 1) {
    
    messageText <- "\n*** There are overlapping time sequences"
    
    message(messageText, ":\n")
    
    print(sequences)
    
    warning("\n", messageText, " (see above and plot).\n")
    
    plotSequenceEvents(dataFrame, sequences, main = main)
    
    FALSE
    
  } else {
    
    TRUE
  }
}

# getOverlappingTimeSequences --------------------------------------------------

#' Get Overlapping Time Sequences
#' 
#' @param timestamps vector of date and time objects, inheriting from
#'   \code{POSIXt}
#' 
#' @return data frame with columns \emph{sequenceNo}, \emph{tBeg}, \emph{tEnd}.
#'   In the attribute "sequenceNumber" the vector of sequenceNumbers, each of
#'   which corresponds to one timestamp is returned.
#' 
getOverlappingTimeSequences <- function(timestamps)
{
  stopifnot(inherits(timestamps, "POSIXt"))
  
  sequenceNumber <- getSequenceNumber(timestamps)
  
  sequenceEvents <- kwb.event::hsEventsOnChange(sequenceNumber)
  
  result <- data.frame(
    sequenceNo = seq_len(nrow(sequenceEvents)),
    sequenceEvents, 
    tBeg = timestamps[sequenceEvents$iBeg],
    tEnd = timestamps[sequenceEvents$iEnd]
  )
  
  structure(result, sequenceNumber = sequenceNumber)
}

# getSequenceNumber ------------------------------------------------------------

#' Get Sequence Number
#' 
#' @param timestamps vector of timestamps
#' 
#' @return vector as long as \emph{timestamps} containing numbers 1, 2, 3, ...
#'   indicating the belonging to a sequence of increasing timestamps. 
#'   If the vector contains only ones, this means that \emph{timestamps} is 
#'   increasingly sorted.
#' 
getSequenceNumber <- function(timestamps)
{
  backShiftAt <- which(diff(as.integer(timestamps)) <= 0)
  
  times <- diff(c(0, backShiftAt, length(timestamps)))
  
  rep(seq_len(length(backShiftAt) + 1), times = times)
}

# plotSequenceEvents -----------------------------------------------------------

#' Plot Sequence Events
#' 
#' @param timestamps vector of timestamps
#' @param sequences data frame as returned by
#'   \code{\link{getOverlappingTimeSequences}} with attribute "sequenceNumber"
#' @param main plot title
#' @param language "de" (German) or something else (English)
#' 
plotSequenceEvents <- function(
  timestamps, sequences, main = "Overlapping time sequences in hydraulic data",
  language = "de"
)
{
  n <- nrow(sequences)
  
  graphicalParameters <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(graphicalParameters))
  
  setMargins(bottom = 10)

  graphics::plot(timestamps, attr(sequences, "sequenceNumber"), ylim = c(n, 1),
       xaxt = "n", 
       yaxt = "n",
       main = main,
       ylab = ifelse(language == "de", "Sequenznummer", "sequence number"), 
       xlab = "")
  
  xPositions <- if (length(timestamps) > 1) {
    
    pretty(timestamps, n = 30)
    
  } else {
    
    timestamps
  }
  
  graphics::abline(v = xPositions, col = "grey", lty = 3)      
    
  graphics::axis.POSIXct(
    side = 1, padj = 0.5, at = xPositions, format = "%d.%m.%y %H:%M", las = 2, 
    cex = 0.6
  )
    
  at <- seq_len(n)
  
  if (length(at) > 11) {
    at <- pretty(at)
  }
  
  graphics::axis(2, at = at, las = 1)
}
