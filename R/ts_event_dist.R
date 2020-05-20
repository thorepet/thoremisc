#' Return the number of rows until a dummy equals 1.
#'
#' Returns a vector of `length(x)` with the number of rows or steps until the
#' next time the dummy series equals 1.
#'
#' @param x A numeric vector of `0`s and `1`s.
#' @return A numeric vector.
#' @examples
#' ts_next_in(c(0, 0, 0, 1, 0, 1, 0))
#' @export
#' @import data.table
ts_next_in <-  function(x) {
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

#' Return the number of rows since a dummy equaled 1.
#'
#' Returns a vector of `length(x)` with the number of rows or steps since the
#' last time the dummy series equaled 1.
#'
#' @param x A numeric vector of `0`s and `1`s.
#' @return A numeric vector.
#' @examples
#' ts_last_before(c(0, 0, 0, 1, 0, 1, 0))
#' @export
#' @import data.table
ts_last_before <-  function(x) {
  # function to return the number of periods since a dummy variable was 1
  # Args:
  #   x: a numeric vector containing 0s and 1s
  # Returns:
  #   a numeric vector of length x containing the periods after the last 1 in x

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

#' Return a information on the occurence of a dummy
#'
#' Returns a data.frame of `length(x)` rows with a column of the number of rows
#' or steps until the next time the dummy series equals 1, and a columns of the
#' number of rows or steps since the last time the dummy series equaled 1.
#'
#' @param x A numeric vector of `0`s and `1`s.
#' @return A data.frame with two numeric columns.
#' @examples
#' ts_event_dist(c(0, 0, 0, 1, 0, 1, 0))
#' @export
ts_event_dist <- function(x) {
  data.frame(last_before = ts_last_before(x), next_in = ts_next_in(x))
}
