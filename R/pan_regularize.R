#' Regularise panel data.
#'
#' Regularise a panel with non-existing rows or timesteps within the series. The
#' panel will be extend per group to the longest temporal coverage across groups
#' to make a balanced panel (at least in the group and time variables). The
#' series will be padded with \code{NA}s.
#'
#' Currently supported are yearly, quaterly, and monthly data.
#'
#' @param x A \code{data.frame} or \code{data.table}.
#' @param g An atomic character, name of the panel group variable in \code{x}.
#' @param t An atomic character, name of the panel time variable in \code{x}.
#' @return A \code{data.frame} or \code{data.table} depending on the input.
#' @examples
#' x <- data.frame(
#'  yr = rep(2000:2002, 2),
#'  grp = rep(c("A", "B"), each = 3),
#'  val = 1:6
#'  )
#' x <- x[-2, ]
#' x_reg <- pan_regularise(x, "grp", "yr")
#' @export
#' @import data.table
#' @importFrom stats median
#' @importFrom zoo as.yearmon as.yearqtr
pan_regularise <- pan_regularize <- function(x, g, t) {
  # define to avoid problems with R CMD check
  V1 <- NULL

  if(is.data.table(x)) {
    isdt <- TRUE
    dt <- x
  } else {
    isdt <- FALSE
    dt <- data.table(x)
  }

  # determine frequency by determining the median difference between steps,
  # requires coding of time variable as year, zoo::yearmon(), or zoo::yearqtr()
  t_steps <- dt[, diff(get(t)), by = g]
  t_steps[, V1 := round(V1, 4)]
  if(median(t_steps$V1) == round(1 / 12, 4)) {
    freq <- "m"
  } else if(median(t_steps$V1) == round(1 / 4, 4)) {
    freq <- "q"
  } else if(median(t_steps$V1) == 1) {
    freq <- "y"
  }

  t_min <- floor(min(dt[, get(t)]))
  t_max <- ceiling(max(dt[, get(t)]))

  if(freq == "m") {
    t_range <- as.yearmon(seq(from = t_min, t_max, by = 1 / 12))
  } else if(freq == "q") {
    t_range <- as.yearqtr(seq(from = t_min, t_max, by = 1 / 4))
  } else if(freq == "y") {
    t_range <- t_min:t_max
  }

  # create regular panel sceleton
  reg <- data.table(expand.grid(unique(dt[, get(g)]), t_range))
  colnames(reg) <- c(g, t)

  # merge main panel with regular sceleton
  res <- merge(reg, dt, by = c(g, t), all = TRUE)

  if(isdt) {
    return(res)
  } else {
    return(as.data.frame(res))
  }
}
