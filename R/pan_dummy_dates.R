#' Create dummy panel from start and end dates.
#'
#' Create a dummy panel over time from data on events with start and end dates.
#' Codes any month, quarter, or year (depending on \code{freq}) during which an
#' event takes place is dummy coded as \code{1}.
#'
#' Currently supported are yearly, quaterly, and monthly data.
#'
#' @param x A \code{data.frame} with events' start and end dates per row.
#' @param g An atomic character, name of the panel group variable in \code{x}.
#' @param col_start Atomic character, name of the column of start dates in
#' \code{x}. As of now, must be of type \code{zoo::yearmon}.
#' @param col_end Atomic character, name of the column of end dates in \code{x}.
#' As of now, must be of type \code{zoo::yearmon}.
#' @param freq Atomic character, the frequency of the resulting panel. One of
#' \code{m} (monthly), \code{q} (quarterly), \code{y} (yearly).
#' @param t_name An atomic character, name of the panel time variable in the
#' resulting \code{data.frame}.
#' @param d_name An atomic character, name of the panel group variable in the
#' resulting \code{data.frame}.
#' @param alt_end Atomic numeric, alternative last period for resulting panel.
#' If \code{NULL}, the resulting panel extends the last date supplied in
#' \code{col_end}.
#' @return A \code{data.frame} or \code{data.table} depending on the input.
#' @examples
#' x <- data.frame(
#'   grp = rep(c("A", "B"), each = 3),
#'   start_month = zoo::as.yearmon(
#'     c("2000-01-01", "2002-05-01", "2003-12-01", "2000-06-01", "2003-01-01",
#'       "2006-01-01")
#'     ),
#'   end_month = zoo::as.yearmon(
#'     c("2000-05-01", "2003-02-01", "2004-03-01", "2001-06-01", "2005-07-01",
#'       "2008-01-01")
#'     )
#' )
#'
#' pdt <- pan_dummy_dates(
#'   x, "grp", "start_month", "end_month", "m", "yearm", "dummy",
#'   zoo::as.yearmon("2008-12-01")
#' )
#' @export
#' @import data.table
#' @importFrom zoo as.yearmon as.yearqtr
pan_dummy_dates <- function(
  x, g, col_start, col_end, freq = c("m", "q", "y"), t_name, d_name,
  alt_end = NULL
) {

  # initialise variables for R NSE check
  d <- NULL

  if(is.data.table(x)) {
    isdt <- TRUE
    dt <- x
  } else {
    isdt <- FALSE
    dt <- data.table(x)
  }

  # get minimum and maximum of all dates as range for final panel
  rt <- as.yearmon(range(as.numeric(c(dt[, get(col_start)],
                                      dt[, get(col_end)]))))

  # make sequence of time indices with correct step size
  t_step <- switch(freq, "m" = 1/12, "q" = 1/4, "y" = 1)

  if(is.null(alt_end)) {
    rt_seq <- seq(rt[1], rt[2], by = t_step)
  } else {
    rt_seq <- seq(rt[1], alt_end, by = t_step)
  }

  res <- data.table(expand.grid(g = unique(dt[, get(g)]), t = rt_seq))
  res[, g := as.character(g)]
  setkey(res, g, t)
  res[, d := 0]

  for(i in 1:nrow(dt)) {
    # characteristics of current event
    grp <- dt[i, get(g)]
    st <- dt[i, get(col_start)]
    et <- dt[i, get(col_end)]

    # sequence of time indices of current event
    td <- seq(st, et, by = t_step)

    # res is keyed, so make use of binary search by group and time index
    # (data.table allows for numerical keys)
    res[list(grp, td), d := 1]
  }
  colnames(res) <- c(g, t_name, d_name)

  return(res)
}
