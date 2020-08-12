#' Distance to dummy-coded events.
#'
#' Given a dummy coded (\code{0} for non-event and \code{1} for event)
#' timeseries, determine the number of periods (vector elements) until the next,
#' and since the last event.
#'
#' @param x A numeric vector of \code{0}s and \code{1}s.
#' @name ts_event_dist
#' @return \code{ts_next_in} and \code{ts_last_before} return a numeric vector
#'   of \code{length(x)}.
#'
#'   \code{ts_event_dist} returns a \code{data.frame} with columns
#'   \code{last_before} and \code{next_in}, and \code{length(x)} rows.
#' @examples
#' ts_next_in(c(0, 0, 0, 1, 0, 1, 0))
#' ts_last_before(c(0, 0, 0, 1, 0, 1, 0))
#' ts_event_dist(c(0, 0, 0, 1, 0, 1, 0))
NULL
#> NULL

#' @export
#' @rdname ts_event_dist
#' @import data.table
ts_next_in <-  function(x) {
  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
  # initialise variables for R NSE check
  event <- event_window <- next_in <- NULL

  dt <- data.table(x)

  dt[x == 1, event := .N:1]

  for(i in 1:max(dt$event, na.rm = TRUE)) {
    row <- which(dt$event == i)
    dt[1:row, event_window := i]
  }

  dt[, next_in := (.N:1) - 1, by = event_window]
  dt[is.na(event_window), next_in := NA]

  return(dt$next_in)
}

#' @rdname ts_event_dist
#' @export
#' @import data.table
ts_last_before <-  function(x) {
  # initialise variables for R NSE check
  event <- event_window <- last_before <- NULL

  dt <- data.table(x = x)

  dt[x == 1, event := 1:.N]

  nr <- nrow(dt)
  for(i in 1:max(dt$event, na.rm = TRUE)) {
    row <- which(dt$event == i)
    dt[row:nr, event_window := i]
  }

  dt[, last_before := (1:.N) - 1, by = event_window]
  dt[is.na(event_window), last_before := NA]

  return(dt$last_before)
}

#' @rdname ts_event_dist
#' @export
ts_event_dist <- function(x) {
  data.frame(last_before = ts_last_before(x), next_in = ts_next_in(x))
}
