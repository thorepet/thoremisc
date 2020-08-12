#' Apply \code{stats::na.contiguous()} and pad result to original length.
#'
#' Apply \code{stats::na.contiguous()} to a timeseries \code{x} and pad the
#' result back to its original length with \code{NA}s. Designed for use in
#' \code{data.table[, j = pad_contiguous(x), by = g]}, where the replacement
#' needs to have the same length as the original.
#'
#' Preserves \code{start} and \code{frequency} of \code{x}, if it is a
#' \code{stats::ts()} object.
#'
#' @param x Either a numeric vector, or \code{stats::ts()} object.
#' @return An object of \code{class(x)} and \code{length(x)} with \code{NA}s,
#' except for the longest contiguous stretch in \code{x}
#' @examples
#' x <- c(NA, 1, NA, NA, 1:5, NA)
#' ts_pad_contig(x)
#' x <- ts(x, start = c(2000, 1), frequency = 12)
#' ts_pad_contig(x)
#' @export
#' @importFrom stats start end is.ts na.contiguous ts
#' @importFrom utils head tail
ts_pad_contig <- function(x) {

  if(all(is.na(x))) return(x)

  if(is.ts(x)) {
    ists <- TRUE
    start <- start(x)
    frequency <- frequency(x)
  } else {
    ists <- FALSE
    start <- 1
    frequency <- 1
    x <- ts(x, start = start, frequency = frequency)
  }

  x_contig <- na.contiguous(x)

  # grab start and end of original and adjusted timeseries
  start_x = start(x)
  end_x = end(x)
  start_contig <- start(x_contig)
  end_contig <- end(x_contig)

  # check if year and quarter of original and adjusted series are the same
  if(!all(start_x == start_contig)){
    # initiate empty timeseries for front padding
    pad_front <- ts(NA, frequency = frequency, start = start_x, end = start_contig)
    # remove the last element, that's already in x_sa
    pad_front <- ts(head(pad_front, -1), frequency = frequency, start = start_x)

    x_contig <- ts(c(pad_front, x_contig), start = start, frequency = frequency)
  }

  if(!all(end_x == end_contig)){
    # initiate empty ts for back padding
    pad_back <- ts(NA, frequency = frequency, start = end_contig, end = end_x)
    # remove first element, that's still in x_sa
    pad_back <- ts(tail(pad_back, -1), frequency = frequency, end = end_x)

    x_contig <- ts(c(x_contig, pad_back), start = start, frequency = frequency)
  }

  if(ists) {
    return(x_contig)
  } else {
    return(as.numeric(x_contig))
  }
}
