# hsFilterRowsWithValuesInColumns ----------------------------------------------

#' Filter for Rows with Given Values in Given Columns
#'
#' Filter for rows that have specified values in specified columns
#'
#' @param dframe data frame
#' @param columnValuePairs list of elements each of which defines a filter
#'   criterion in the form <column-name> = <value>
#'
#' @return data frame containing those rows of \emph{dframe} that comply with
#'   all of the filter criteria defined in \emph{columnValuePairs}
#' @importFrom kwb.utils printIf
#' 
hsFilterRowsWithValuesInColumns <- function(dframe, columnValuePairs)
{
  for (columnName in names(columnValuePairs)) {
    
    selected <- (dframe[[columnName]] == columnValuePairs[[columnName]])
    dframe <- dframe[selected, ]
  }
  
  dframe
}

# .test_hsLabValToVal ----------------------------------------------------------
.test_hsLabValToVal <- function(ver = 1, N = 1)
{
  labvalues_1 <- c(
    "1,0", "1,00", "1,000", "1.000", "1.000,123", 
    "<0,1", "  <0,1", "  <  0,1", "> 1,23", " < 1.234.567,890",
    "", "  "
  )
  
  labvalues_2 <- c("<<1000", ">>2000", " < < 1000", "  > >  123,45")
  
  expected_1 <- data.frame(
    outOfLimit = c("", "", "", "", "",
                   "<", "<", "<", ">", "<",
                   "", ""),
    numericValue = c(1.0, 1.0, 1.0, 1000, 1000.123,
                     0.1/2, 0.1/2, 0.1/2, 1.23*2, 1234567.890/2,
                     NA, NA),
    stringsAsFactors = FALSE
  )
  
  expected_2 <- data.frame(
    outOfLimit = c("<<", ">>", "<<", ">>"),
    numericValue = c(1000/2, 2000*2, 1000/2, 123.45*2),
    stringsAsFactors = FALSE
  )
  
  if (ver == 1) {
    labvalues <- labvalues_1
    expected <- expected_1
  } else {
    labvalues <- c(labvalues_1, labvalues_2)
    expected <- rbind(expected_1, expected_2)
  }
  
  labvalues <- rep(labvalues, N)
  expected <- expected[rep(seq_len(nrow(expected)), N), ]
  row.names(expected) <- NULL
  
  kwb.utils::printIf(TRUE, system.time(y_old <- hsLabValToVal_old(
    labvalues, country = "de", stopOnError = FALSE)), "old")
  
  kwb.utils::printIf(TRUE, system.time(y_new <- hsLabValToVal(
    labvalues, country = "de")), "new")
  
  c(old_version = identical(y_old, expected),
    new_version = identical(y_new, expected))
}

# hsLabValToVal ----------------------------------------------------------------

#' Laboratory Value to Numeric Value
#'
#' conversion of text representing a number, but possibly starting with "<" or
#' ">" to indicate that the number exceeds the detection limit of an analysis
#' method, to a valid number
#'
#' @param x (vector of) character(s) representing values, possibly starting with
#'   "<" or ">" to indicate detection limit exceedance
#' @param country "en" if value is given in English format (decimal point ".",
#'   thousands separator ",") or "de" if value is given in German format
#'   (decimal point ",", thousands separator ".").
#' @param detLimFactorBelow factor by which detection limit is multiplied in
#'   order to get a valid value when the value was below the detection limit.
#'   Default value: 0.5
#' @param detLimFactorAbove factor by which detection limit is multiplied in
#'   order to get a valid value when the value was above the detection limit.
#'   Default value: 2
#' @param factors named vector of conversion factors to be multiplied with the
#'   numeric values if the name of the factor matches the prefix (e.g. "<",
#'   "<<", ">", ">>") found in front of the value. Set to NULL if not factors
#'   are to be applied
#' @param stopOnError if TRUE, the program stops on conversion errors, otherwise
#'   shows a warning
#'
#' @return data frame with columns \emph{outOfLimit} being one of "" (value
#'   within detection limits), "<" (value below detection limit) or ">" (value
#'   above detection limit) and \emph{numericValue} containing the value which,
#'   in case of detection limit exceedance, may be a substitute value. If there
#'   were conversion errors, the column \emph{numericValue} containing the
#'   indices of the wrongly formatted values in its attribute "errorIndices".
#' @importFrom kwb.utils hsChrToNum
hsLabValToVal <- function(
  x, country, detLimFactorBelow = 0.5, detLimFactorAbove = 2,
  factors = c("<" = detLimFactorBelow, "<<" = detLimFactorBelow, 
              ">" = detLimFactorAbove, ">>" = detLimFactorAbove),
  stopOnError = TRUE
)
{  
  x <- gsub("\\s+", "", x) # remove spaces
  
  ## set start and stop positions for substr  
  startIndex <- rep(1, length(x))
  stopIndex <- nchar(x)
  
  ## indices of elements starting with "<" or ">"
  indices_1 <- grep("^(<|>)[^<>]", x)
  indices_2 <- grep("^(<<|>>)[^<>]", x)
  
  startIndex[indices_1] <- 2
  startIndex[indices_2] <- 3
  
  # (vector of) characters containing the value(s)
  textValues <- substr(x, startIndex, stopIndex) 
  
  ## set vector containing "<", ">" or ""
  N <- length(x)
  outOfLimit <- rep("", N)
  
  outOfLimit[indices_1] <- substr(x[indices_1], 1, 1)  # 1st character
  outOfLimit[indices_2] <- substr(x[indices_2], 1, 2)  # 1st two characters
  
  ## set vector containing values  
  numericValue <- kwb.utils::hsChrToNum(textValues, country, stopOnError = stopOnError)  
  
  ## Multiply values with factors if value was out of detection limits
  multiplyWith <- rep(1, N)
  
  for (prefix in names(factors)) {
    
    multiplyWith[outOfLimit == prefix] <- factors[prefix]
  }
  
  data.frame(
    outOfLimit = outOfLimit, 
    numericValue = numericValue * multiplyWith, 
    stringsAsFactors = FALSE
  )
}

# hsLabValToVal_old ------------------------------------------------------------

#' Laboratory Value to Numeric Value (Old Version)
#'
#' Conversion of text representing a number, but possibly starting with "<" or
#' ">" to indicate that the number exceeds the detection limit of an analysis
#' method, to a valid number
#'
#' @param x (vector of) character(s) representing values, possibly starting with
#'   "<" or ">" to indicate detection limit exceedance
#' @param country "en" if value is given in English format (decimal point ".",
#'   thousands separator ",") or "de" if value is given in German format
#'   (decimal point ",", thousands separator ".").
#' @param detLimFactorBelow factor by which detection limit is multiplied in
#'   order to get a valid value when the value was below the detection limit.
#'   Default value: 0.5
#' @param detLimFactorAbove factor by which detection limit is multiplied in
#'   order to get a valid value when the value was above the detection limit.
#'   Default value: 2
#' @param stopOnError if TRUE, the program stops on conversion errors, otherwise
#'   shows a warning
#'
#' @return data frame with columns \emph{outOfLimit} being one of "" (value
#'   within detection limits), "<" (value below detection limit) or ">" (value
#'   above detection limit) and \emph{numericValue} containing the value which,
#'   in case of detection limit exceedance, may be a substitute value. If there
#'   were conversion errors, the column \emph{numericValue} containing the
#'   indices of the wrongly formatted values in its attribute "errorIndices".
#' @importFrom kwb.utils hsChrToNum
hsLabValToVal_old <- function(
  x, country, detLimFactorBelow = 0.5, detLimFactorAbove = 2, stopOnError = TRUE
)
{  
  #x.backup <- x
  
  x <- gsub("\\s+", "", x) # remove spaces
  
  ## indices of elements starting with "<" or ">"
  indices <- grep("^[<>]", x)
  firstCharacter <- substr(x, 1, 1)  # 1st character
  
  ## set start and stop positions for substr  
  startIndex <- rep(1, length(x))
  startIndex[indices] <- 2
  stopIndex <- nchar(x)    
  
  # (vector of) characters containing the value(s)
  textValues <- substr(x, startIndex, stopIndex) 
  
  ## set vector containing "<", ">" or ""
  outOfLimit <- rep("", length(x))
  outOfLimit[indices] <- firstCharacter[indices]
  
  ## set vector containing values  
  numericValue <- kwb.utils::hsChrToNum(textValues, country, stopOnError = stopOnError)  
  
  ## Multiply values with factors if value was out of detection limits
  below <- outOfLimit == "<"
  above <- outOfLimit == ">"
  
  numericValue[below] <- numericValue[below] * detLimFactorBelow
  numericValue[above] <- numericValue[above] * detLimFactorAbove
  
  data.frame(outOfLimit, numericValue, stringsAsFactors = FALSE)
}

# hsDelNaRowsOrCols ------------------------------------------------------------

#' Delete Rows or Columns Containing only NA
#' 
#' @param df data frame
#' @param rows if TRUE, rows that only contain NAs are deleted, else columns.
#' @param drop if TRUE and only one row/column remains this row/column is returned
#'   in forms of a vector instead as a data frame.
#' @importFrom kwb.utils isNaInAllColumns isNaInAllRows isNullOrEmpty
hsDelNaRowsOrCols <- function(df, rows = TRUE, drop = FALSE)
{  
  ## Find rows that are NA in all columns
  idx <- NULL
  
  rowcol <- ifelse(rows, "rows", "columns")
  
  if (isTRUE(rows)) {
    
    cat("Removing rows that contain only NAs...\n")
    idx <- which(kwb.utils::isNaInAllColumns(df))
    
  } else {
    
    cat("Removing columns that contain only NAs...\n")
    idx <- which(kwb.utils::isNaInAllRows(df))
  }
  
  ## Remove NA rows/columns
  if (! kwb.utils::isNullOrEmpty(idx)) {
    
    if (isTRUE(rows)) {
      
      ## Remove NA rows
      df <- df[-idx, , drop = drop]
      
    } else {
      
      ## Remove NA columns
      df <- df[, -idx, drop = drop]              
    }
    
    cat(sprintf("%d NA-%s removed.\n", length(idx), rowcol))
    
  } else{
    
    cat(sprintf("No %s found that contains only NA values.\n", rowcol))
  }
  
  df
}

# hsDbTablePlotXY --------------------------------------------------------------

#' X-Y Plot of Two Database Table Fields
#' 
#' Plots the values of two database fields (= columns) against each other and
#' prints the plot into a PDF file.
#'
#' @param strDb full path to MS Access database file (*.mdb)
#' @param strTable name of table in database
#' @param strX name of table field containing the "x" values
#' @param strY name of table field containing the "y" values
#' @param strPdfFile full path to PDF file to be generated. If omitted, the plot
#'   is shown on the screen.
#'
#' @return If the output device is a pdf file the result of the dev.off()
#'   command is returned.
#'
#' @examples
#' \dontrun{
#' ## Plot CSB vs. timestamp values from table "tbl_STA_CAL" in the
#' ## example database into a window on the screen
#' hsDbTablePlotXY(kwb.db::xmdb(), "tbl_Qua", "myDateTime", "CSB")
#'
#' ## Set paths to a test PDF file
#' pdf_file <- file.path(tempdir(), "ex_hsDbTablePlotXY.pdf")
#'
#' ## Plot CSBf vs. timestamp values from the same table into a PDF file.
#' hsDbTablePlotXY(kwb.db::xmdb(), "tbl_Qua", "myDateTime", "CSBf", pdf_file)
#'
#' ## Open PDF file in PDF viewer
#' kwb.utils::hsShowPdf(pdf_file)
#' }
#' @importFrom kwb.db hsSqlQuery
#' @importFrom grDevices dev.off 
#' @importFrom graphics plot
hsDbTablePlotXY <- function(strDb, strTable, strX, strY, strPdfFile = NULL)
{
  # Generate SQL string
  sql <- sprintf("SELECT t.[%s], t.[%s] FROM %s AS t", strX, strY, strTable)
  
  #@2011-12-21: hsSqlQuery instead of odbcConnectAccess, sqlQuery, odbcClose  
  res <- kwb.db::hsSqlQuery(strDb, sql)
  
  # Open pdf device if file is given
  if (!is.null(strPdfFile)) {
    
    grDevices::pdf(file = strPdfFile)
  }
  
  graphics::plot(res[[strX]], res[[strY]], xlab=strX, ylab=strY)
  
  # Close device
  if (! is.null(strPdfFile)) {
    
    grDevices::dev.off()
  }
}

# hsFilterPeriod ---------------------------------------------------------------

#' Filter Rows Within Time Period
#'
#' Filters a data.frame containing time series data for the time interval
#' between \emph{minDate} and \emph{maxDate}.
#'
#' \emph{minDate} and \emph{maxDate} must be given as character string in ISO
#' format: \dQuote{yyyy-mm-dd [HH:MM:SS]}, where the brackets indicate that time
#' information is optional. The lower boundary is always included in the time
#' interval to be selected, whereas the upper boundary is only included if
#' \emph{maxIncluded} is TRUE and otherwise excluded. With \emph{maxIncluded} =
#' FALSE it is easy to select whole months or whole years by setting
#' \emph{maxDate} to the first day of the next month/year.
#' 
#' @param tSeries data.frame containing time-series data.
#' @param minDate lower boundary of time interval to be selected, as either of
#'   POSIXt-object, Date object or string in ISO format: yyyy-mm-dd
#' @param maxDate upper boundary of time interval to be selected, as either of
#'   POSIXt-object, Date object or string in ISO format: yyyy-mm-dd
#' @param tsField name of time stamp column in \emph{tseries}.
#' @param maxIncluded if TRUE, \emph{maxDate} is included in the time interval
#'   to be filtered for, otherwise it is not included (see Details).
#' @param dbg if \code{TRUE} debug messages are shown
#' 
#' @return Returns \emph{tSeries}, reduced to rows representing a time within
#'   the selected time interval between \emph{minDate} and \emph{maxDate}
#' @importFrom kwb.utils catIf
#' @importFrom kwb.datetime hsToPosix
hsFilterPeriod <- function(
  tSeries, minDate, maxDate, tsField, maxIncluded = FALSE, dbg = FALSE
)
{
  ## tSeries must be of class data.frame
  if (class(tSeries) != "data.frame") {
    
    stop("tSeries must be of class data.frame.\n")
  }
  
  ## If no name is given for the timestamp field try the first 
  ## column
  if (missing(tsField)) tsField = names(tSeries)[1]
  
  ## Times in data.frame must be in UTC, otherwise they cannot be
  ## compared to the boundaries of the time interval.
  if (! class(tSeries[[tsField]])[1] %in% c("POSIXt", "POSIXct", "POSIXlt"))
    stop(sprintf("Time stamp column '%s' is not of class POSIXt as expected!\n",
                 tsField))
  
  ## Time zone must be UTC
  tz <- attr(tSeries[[tsField]], "tzone")
  if (is.null(tz) || tz != "UTC") 
    stop(sprintf("Timestamp column '%s' is not in UTC time zone as expected!\n",
                 tsField))
  
  # If minDate and/or maxDate are given, filter rain data for the given 
  # period between minDate and maxDate
  if (! missing(minDate)) {
    
    utcMin <- kwb.datetime::hsToPosix(minDate) #hsUTC(minDate, posixLt = TRUE) #as.POSIXlt(minDate, tz="UTC")
    
    kwb.utils::catIf(dbg, sprintf("Filter dates >= %s\n", minDate))
    
    # Filter for rows where timestamp >= minDate
    tSeries <- tSeries[tSeries[[tsField]] >= utcMin, ]
  }
  
  if (! missing(maxDate)) {
    
    utcMax <- kwb.datetime::hsToPosix(maxDate) #hsUTC(maxDate, posixLt = TRUE) # as.POSIXlt(maxDate, tz="UTC")
    
    kwb.utils::catIf(dbg, sprintf(
      "Filter dates %s %s\n", ifelse(maxIncluded, "<=", "<"), maxDate
    ))
    
    tSeries <- if (maxIncluded) {
      
      # Filter for rows where timestamp <= maxDate
      tSeries[tSeries[[tsField]] <= utcMax, ]
      
    } else {
      
      # Filter for rows where timestamp < maxDate
      tSeries <- tSeries[tSeries[[tsField]] < utcMax, ]
    }
  }
  
  # kwb.utils::printIf(dbg, idx, "Indices")
  
  # If indices are given, reduce data.frame to these indices
  # if (! is.null(idx)) tSeries <- tSeries[idx,]
  
  kwb.utils::printIf(
    dbg, utils::head(tSeries), "*** First records of filtered time series"
  )
  
  kwb.utils::printIf(
    dbg, utils::tail(tSeries), "\n*** Last records of filtered time series"
  )
  
  tSeries
}

# hsPlot -----------------------------------------------------------------------

#' Plot to Specific Device
#'
#' Generic function for redirecting a plot-command to a specific device.
#'
#' @param dev device id
#' @param plotFun plot-function to be used, e.g. \code{plot}, \code{barplot},
#'   \code{points}, \code{lines}, ...
#' @param args list of arguments to be passed to the function given in
#'   \emph{plotFun}
#'
#' @return This function returns whtat the plot function given in \code{plotFun}
#'   returns
#' @importFrom grDevices dev.cur dev.set
hsPlot <- function(dev, plotFun = graphics::plot, args)
{
  cat("in hsPlot()... ")
  
  ## save current device
  devCur <- grDevices::dev.cur()
  
  ## set device to dev
  grDevices::dev.set(dev)
  
  ## plot to device
  ret <- do.call(plotFun, args)
  
  ## restore old device
  grDevices::dev.set(devCur)
  
  cat("end of hsPlot().\n")
  
  ret
}

# hsPdfDev ---------------------------------------------------------------------

#' ID(s) of PDF Device(s)
#'
#' Returns the IDs of the first or all opened pdf device(s).
#'
#' @param all if TRUE, the ids of all pdf devices are returned, if FALSE, only
#'   the id of the first pdf device
#'
#' @return ID of first (\emph{all} == FALSE) or IDs of all opened pdf devices,
#'   as e.g. returned by \code{\link{dev.list}}
#' @importFrom grDevices dev.list   
hsPdfDev <- function(all = FALSE)
{
  dl <- grDevices::dev.list()
  dl <- dl[names(dl) == "pdf"]
  
  if (length(dl) == 0) {
    
    return (0)
  }
  
  if (all) {
    
    return(dl)    
  }
  
  return(dl[1])  
}

# hsGroupBy2Fields -------------------------------------------------------------

#' Two-field Grouping of a Data Frame
#' 
#' Groups data by values in two columns creating a matrix with as many rows as
#' there are distinct values in field1 and as many columns as there are distinct
#' values in field2. The matrix contains the sum of values in the specified
#' value field that belong to the corresponding value combination of
#' field1/field2
#'
#' @param frmData data.frame containing data to be grouped.
#' @param strValField name of value field (= column in frmData)
#' @param strField1 name of first field to be grouped by
#' @param strField2 name of second field to be grouped by
#' @param boolDesc1 if TRUE, columns in result matrix will be ordered according
#'   to decreasing values of field1
#' @param boolDesc2 if TRUE, rows in result matrix will be ordered according to
#'   decreasing values of field2
#'
#' @return matrix with as many rows as there are distinct values in field1 and
#'   as many columns as there are distinct values in field2 of the input
#'   data.frame. The matrix contains the sum of values in the specified value
#'   field of the input data.frame that belong to the corresponding combination
#'   of values in field1/field2
#' 
hsGroupBy2Fields <- function(
  frmData, strValField, strField1, strField2, boolDesc1 = FALSE, 
  boolDesc2 = FALSE
)
{
  # Vector of distinct values in field 1
  vecField1 <- sort(
    as.numeric(levels(as.factor(frmData[[strField1]]))), decreasing = boolDesc1
  )
  
  # Vector of distinct values in field 2
  vecField2 <- sort(
    as.numeric(levels(as.factor(frmData[[strField2]]))), decreasing = boolDesc2
  )
  
  # Prepare result matrix with rows representing distinct values of field 1 
  # and columns representing distinct values of field 2
  matRes <- matrix(
    nrow = length(vecField1), 
    ncol = length(vecField2), 
    dimnames = list(vecField1, vecField2)
  )
  
  # Loop through distinct values of field 1
  for (int1 in seq_along(vecField1)) {
    
    # Loop through distinct values of field 2
    for (int2 in seq_along(vecField2)) {
      
      # Vector of indices of rows representing the combination of values in
      # field 1/field 2
      vecInd <- (
        (frmData[[strField1]] == vecField1[int1]) & 
          (frmData[[strField2]] == vecField2[int2])
      )
      
      # Create vector of values representing the current combination 
      # of values of field 1 field 2.
      # This vector should always contain 0 or 1 element
      vecVal <- frmData[[strValField]][vecInd]
      
      if (length(vecVal) > 1) {
        
        print(paste(
          "Warning: More than one rows found with", strField1, 
          "=", vecField1[int1], "and", strField2, "=", vecField2[int2], "!"
        ))
      }
      
      # Save (sum of) value(s) (or 0 if NA) in result matrix
      matRes[int1, int2] <- ifelse(length(vecVal) == 0, 0, sum(vecVal))      
    }
  }
  
  matRes
}

# hsMfRows ---------------------------------------------------------------------

#' Needed Rows for mfrow
#'
#' Number of rows needed to plot <nPlots> in a grid with <plotCols> plots per
#' row.
#'
#' @param nPlots number of total plots
#' @param nPlotsPerRow number of plots per row
#'
#' @return Number of rows needed to place all the plots.
#'   
hsMfRows <- function(nPlots, nPlotsPerRow) 
{
  as.integer((nPlots - 1) / nPlotsPerRow) + 1
}

# hsWait -----------------------------------------------------------------------

#' Wait for Specified Time
#' 
#' Waits for the specified number of seconds.
#' 
#' @param secs number of seconds to wait
#' 
hsWait <- function(secs = 1)
{
  t <- Sys.time()
  
  while (as.double(Sys.time() - t) < secs) {
    
  }
}

# artificialHydrograph ---------------------------------------------------------

#' Artificial Hydrograph
#' 
#' Generates an artificial hydrograph
#' 
#' @param step.s time step in seconds
#' @param from first day as character string in format "yyyy-mm-dd"
#' @param to last day as character string in format "yyyy-mm-dd"
#' 
#' @return data frame with columns \emph{DateTime} and \emph{values}
#' @importFrom kwb.datetime sequenceOfTimestamps toGmtRelativePosix
#' @importFrom stats rnorm
artificialHydrograph <- function(
  step.s = 3600, from = "2015-01-01", to = "2015-01-10"
) 
{
  FUN <- function(x) {
    
    x <- as.integer(times) / (12 * 3600)
    
    n <- length(x)
    
    200 + stats::rnorm(n, mean = 100, sd = 50) * sin(x) + 
      stats::rnorm(n, mean = 1) * cos(10*x)
  }
  
  times <- kwb.datetime::toGmtRelativePosix(kwb.datetime::sequenceOfTimestamps(from, to, step.s = step.s))
  
  data.frame(DateTime = times, values = FUN(times))
}

# hsExampleTSeries -------------------------------------------------------------

#' Data Frame with Example Time Series
#' 
#' Example time series
#' 
#' @param step time step in seconds
#' 
#' @return data frame with columns \emph{t} (timestamp) and \emph{y} (sinus values)
#' @export
#' @importFrom kwb.datetime hsToPosix
hsExampleTSeries <- function(step)
{
  ## Generate a data frame containing a sinus time series with
  ## values at every 5 minutes
  t0 <- kwb.datetime::hsToPosix("2012-01-01 12:00:00")
  t1 <- kwb.datetime::hsToPosix("2012-01-01 12:20:00")
  
  ts <- seq(from = t0, to = t1, by   = step)
  
  rng <- as.integer(difftime(t1, t0, units = "s"))
  ts0 <- as.integer(difftime(ts, t0, units = "s"))
  
  fx <- sin(2 * pi * ts0 / rng)
  
  data.frame(t = ts, y = fx)  
}

# demoGroupByInterval ----------------------------------------------------------

#' Demonstrate hsGroupByInterval
#'
#' Creates a plot demonstrating the effect of offset1 and offset2 in function
#' hsGroupByInterval
#'
#' @param df data frame containing data to be used for the demonstration
#' @param step time step in seconds
#' @param to_pdf if \code{TRUE} the output goes into a PDF file
#' @importFrom kwb.datetime minTimeStep
#' @importFrom kwb.utils preparePdfIf finishAndShowPdfIf
#' @importFrom graphics abline axis par plot points legend
demoGroupByInterval <- function(
  df = hsExampleTSeries(60), 
  step = kwb.datetime::minTimeStep(df[, 1]),
  to_pdf = TRUE
)
{
  pdf_file <- kwb.utils::preparePdfIf(to_pdf)
  
  ## Prepare plot area for two plots below each other
  old_par <- graphics::par(mfrow = c(2, 1))
  on.exit(graphics::par(old_par))
  
  ## Show effect of offset1 in first plot  
  graphics::plot(
    df$t, df$y, type = "o", pch = 16, axes = FALSE, xlab = NA, ylab = NA,
    main = paste(
      "offset1 shifts the limits (dashed lines)", 
      "of the time intervals",
      "\n(here: time interval length ti = 10 min;",
      "offset2 = ti/2 in each case)"
    )
  )
  
  graphics::axis(2)
  graphics::axis(1, at = df$t, labels = format(df$t, "%H:%M"))
  
  ## Group values lying in intervals of 10 minutes...
  tpi <- 10 ## Timestamps per interval
  ti <- tpi * step
  
  # Call hsGroupByInterval with common arguments and one changing argument
  df1 <- lapply(c(a = 0, b = ti/3, c = ti/2), function(offset1) {
    
    hsGroupByInterval(df, ti, mean, limits = TRUE, offset1 = offset1)
  })
  
  ## Plot vertical lines at interval limits
  n <- nrow(df)
  xshift <- 0# step/2

  colours <- c("red", "blue", "green")

  # Add vertical lines and points
  for (i in 1:3) {

    graphics::abline(
      v = c(df1[[i]]$t.beg, df1[[i]]$t.end[n]) - xshift, col = colours[i],
      lty = 2, lwd = 2
    )
    
    graphics::points(
      df1[[i]]$t, df1[[i]]$y, col = colours[i], type = "o", pch = 16
    )
  }

  # Add legend
  graphics::legend(
    "topright", fill = c("red", "blue", "green"), 
    legend = paste("offset1", c("0", "ti/3", "ti/2"), sep = " = ")
  )
  
  ## Show effect of offset2 in second plot
  graphics::plot(
    df$t, df$y, type = "o", pch = 16, axes = FALSE, xlab = NA, ylab = NA,
    main = paste(
      "offset2 selects the timestamp representing the interval",
      "\n(here: time interval length ti = 10 min;",
      "offset1 = 0 in each case)"
    )
  )
  
  graphics::axis(2)
  graphics::axis(1, at = df$t, labels = format(df$t, "%H:%M"))
  
  df2 <- list(
    a = df1$a,
    b = hsGroupByInterval(df, ti, mean, offset1 = 0, offset2 = ti/3),
    c = hsGroupByInterval(df, ti, mean, offset1 = 0, offset2 = 0)
  )
  
  ## Plot vertical lines at interval limits
  graphics::abline(
    v = c(df2$a$t.beg, df2$a$t.end[n]) - xshift, col = "grey", lty = 2, lwd = 2
  )
  
  for (i in 1:3) {
    
    graphics::points(
      df2[[i]]$t, df2[[i]]$y, type = "o", pch = 16, col = colours[i]
    )
  }
  
  graphics::legend(
    "topright", fill = c("red", "blue", "green"), 
    legend = paste("offset2", c("ti/2", "ti/3", "0"), sep = " = ")
  )
  
  kwb.utils::finishAndShowPdfIf(to_pdf, pdf_file)
}

# hsGroupByInterval ------------------------------------------------------------

#' Group Data in Time Intervals
#'
#' Builds groups of rows belonging to the same time interval and aggregates the
#' values within the group by using a given function (e.g. sum, mean, min, max)
#'
#' @param data data frame containing a timestamp field and data fields to be
#'   aggregated over time.
#' @param interval length of time interval in seconds
#' @param FUN function used to aggregate the values within one and the same
#'   interval, e.g. sum, mean, min, max
#' @param tsField name of timestamp column, default: name of first column
#' @param offset1 number of seconds by which all timestamps are shifted before
#'   they are grouped into intervals. The grouping to intervals is done by
#'   dividing the timestamps (converted to number of seconds since 1970-01-01)
#'   by the interval length and taking the integer part of the division as
#'   interval number. Thus, with offset1 = 0 and an interval length of e.g. 60
#'   seconds, the first interval is from 00:00:00 to 00:00:59, the second from
#'   00:01:00 to 00:01:59 etc., whereas offset1 = 30 in this case would lead to
#'   intervals 00:00:30 to 00:01:29, 00:01:30 to 00:02:29 etc..
#' @param offset2 value given in seconds determining which of the timestamps in
#'   an interval represents the interval in the output. If 0, each time interval
#'   is represented by the smallest timestamp belonging to the interval. By
#'   default, offset2 is half the interval length, meaning that each time
#'   interval is represented by the timestamp in the middle of the interval.
#' @param limits if TRUE, two additional columns will be added showing the
#'   minimum and maximum value of the interval
#' @param \dots further arguments passed to aggregate, the internally called
#'   function
#' @param dbg if TRUE, debug messages are shown
#'
#' @examples
#' 
#' ## Get an example time-series with values every one minute
#' step <- 60
#' df <- hsExampleTSeries(step)
#'
#' ## Calculate 5-min-means with
#' ## offset1 = 0 (default), offset2 = interval/2 (default)
#' df.agg1 <- hsGroupByInterval(df, interval = 5*step, mean, limits = TRUE)
#' df.agg1
#'
#' ## Shift the interval limits with
#' ## offset1 = 2.5*60, offset2 = interval/2 (default)
#' df.agg2 <- hsGroupByInterval(df, interval = 5*step, mean, limits = TRUE,
#'                              offset1 = 2.5*step)
#' df.agg2
#'
#' ## Shift the timestamps representing the intervals with
#' ## offset1 = 0, offset2 = 0
#' df.agg3 <- hsGroupByInterval(df, interval = 5*step, mean, limits = TRUE,
#'                              offset1 = 0, offset2 = 0)
#' df.agg3
#'
#' ## Show a plot demonstrating the effect of offset1 and offset2
#' \dontrun{
#' demoGroupByInterval(df)
#'}
#' ## Handling NA values...
#'
#' ## Set y to NA at 2 random positions
#' df[sample(nrow(df), 2), 2] <- NA
#' df ## Let' have a look at df
#'
#' ## Count NA values per group
#' hsGroupByInterval(df, interval = 300, function(x){sum(is.na(x))})
#'
#' ## default behaviour: mean(values containing at least one NA) = NA
#' hsGroupByInterval(df, interval = 300, mean)
#'
#' ## ignore NA values by passing na.rm = TRUE to the aggregate function
#' hsGroupByInterval(df, interval = 300, mean, na.rm = TRUE)
#' @importFrom stats aggregate
#' @importFrom utils head
hsGroupByInterval <- function(
  data, interval, FUN, tsField = names(data)[1], offset1 = 0, 
  offset2 = interval / 2, limits = FALSE, ..., dbg = FALSE
)
{
  ## data must be of type data.frame
  if (class(data) != "data.frame") {
    
    stop("data must be of type data.frame")
  }
  
  ## Field name must exist and must be of POSIX-type
  if (! (tsField %in% names(data))) {
    
    stop("Field with name '", tsField, "' does not exist in data.")
  }

  fclass <- class(data[[tsField]])
  
  if (length(intersect(fclass, c("POSIXct", "POSIXlt", "POSIXt"))) == 0) {
    
    stop(
      "Field '", tsField, "' is not of POSIX-type but of class '", fclass, "'."
    )
  }
  
  ## Convert timestamps to integer, shift by offset1 and divide by interval 
  ## length in order to receive an interval id
  grpid <- (as.integer(data[[tsField]]) - offset1) %/% interval
  
  ## Aggregate values by grpid
  by <- list(grpid)
  names(by) <- tsField
  
  ## number of timestamp column
  tsFieldNo <- (1:ncol(data))[names(data) == tsField]
  
  ## aggregate by group ids
  res <- stats::aggregate(
    data[, -tsFieldNo, drop = FALSE], by = by, FUN = FUN, ...
  )
  
  kwb.utils::printIf(
    dbg, utils::head(res, 10), "result of aggregation (first 10)"
  )

  ## Replace group ids by timestamps
  t1 <- data[[tsField]][1]
  tz <- attr(t1, "tzone")
  
  orig <- "1970-01-01" # as.character(t1 - as.integer(t1))
  
  interval.beg <- unique(grpid) * interval + offset1
  
  res[[tsField]] <- as.POSIXct(
    interval.beg + offset2, tz = tz, origin = orig)
  
  kwb.utils::printIf(
    dbg, utils::head(res, 10), "\nresult after assigning timestamps (first 10)"
  )

  if (isTRUE(limits)) {
    
    tsField.min <- paste(tsField, "beg", sep = ".")
    tsField.max <- paste(tsField, "end", sep = ".")
    
    interval.end <- interval.beg + interval - 1
    
    kwb.utils::catIf(dbg, "Class of res before cbind:", class(res), "\n")

    ## Bind together: timestamp column, columns for begin and end of interval,
    ## data columns
    res <- cbind(
      res[, 1, drop = FALSE], 
      as.POSIXct(interval.beg, tz = tz, origin = orig),
      as.POSIXct(interval.end, tz = tz, origin = orig),
      res[, -1, drop = FALSE]
    )
    
    kwb.utils::catIf(
      dbg, "Class of res after cbind (first 10):", class(res), "\n"
    )

    names(res)[1:3] <- c(tsField, tsField.min, tsField.max)
  }
  
  res
}

# hsNiceLabels -----------------------------------------------------------------

#' Nice Labels
#'
#' Generates a nice vector of labels by suppressing labels at certain positions.
#' Please use kwb.plot::niceLabels instead
#' 
#' @param label see \code{\link[kwb.plot]{niceLabels}}
#' @param labelstep see \code{\link[kwb.plot]{niceLabels}}
#' @param labelpos see \code{\link[kwb.plot]{niceLabels}}
#' @param mindist see \code{\link[kwb.plot]{niceLabels}}
#' @param offset see \code{\link[kwb.plot]{niceLabels}}
#' @importFrom  kwb.plot niceLabels
#' @importFrom kwb.utils warningDeprecated
hsNiceLabels <- function(
  label, labelstep = NULL, labelpos = NULL, mindist = 1, offset = 0
)
{
  kwb.utils::warningDeprecated("kwb.base::hsNiceLabels", "kwb.plot::niceLabels")

  kwb.plot::niceLabels(
    label = label, 
    labelstep = labelstep, 
    labelpos = labelpos, 
    mindist = mindist, 
    offset = offset
  )
}
