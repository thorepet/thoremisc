#' Labeller function for \code{ggplot2} \code{facet}s.
#'
#' Takes a character vector of some origin format given in
#' \code{?countrycode::codelist} and returns either a character vector of the
#' format [origin] - [destination], or a character vector of only the
#' destination format.
#'
#' @param x A character vector.
#' @param origin An atomic character, see \code{?countrycode::codelist}.
#' @param destination An atomic character, see \code{?countrycode::codelist}.
#' @param both Boolean, should origin and destination be returned, or only
#' destination?
#' @return A character vector.
#' @examples
#' pan_labeller_country(c("AUS", "TWN"))
#' @export
#' @importFrom countrycode countrycode
pan_labeller_country <- function(
  x, origin = "iso3c", destination = "country.name", both = TRUE
) {
  if(both) {
    paste(
      x,
      countrycode(sourcevar = x, origin = origin, destination = destination),
      sep = " - "
    )
  } else {
    countrycode(sourcevar = x, origin = origin, destination = destination)
  }
}
