#' Count and ennumerate of events in a dummy series.
#'
#' Given a numeric vector with dummies for events, ennumerate the events. Return
#' a vector of \code{length(x)} filled with \code{NA}s, and the count, where
#' \code{x} equals \code{1}.
#'
#' @param x A numeric vector of \code{0}s and \code{1}s.
#' @return A numeric vector containing \code{NA}s, and the count of the event
#' corresponding to the dummy series.
#' @examples
#' ts_event_count(c(0, 0, 1, 1, 0, 1, 0))
#' @export
#' @import data.table
ts_event_count <- function(x) {

  # initialise variables for R NSE check
  ec <- epd <- event <- NULL

  if(any(is.na(x))) stop("Can not handle NAs in the dummy series.")

  dt <- data.table(event = x)

  # event positive difference equals 1 when event starts, 0 else
  dt[, epd := c(NA, pmax(diff(event), 0))]

  # if first period is an event, set epd = 1 "manually", otherwise NA
  if(dt[1, event] == 1) dt[1, epd := 1]

  # cumsum of event start indicators in event rows gives numbering of events
  dt[event == 1, ec := cumsum(epd)]

  return(dt[, ec])
}
