#' Utiliy function to create Regex pattern.
#' 
#' Utility function to collapse a character vector into a Regex pattern. The
#' vector elements will be surrounded by word boundaries, and combined with
#' logical ORs. The whole thing is wrapped in parantheses to catch it as a
#' group.
#'
#' @param x A character vector.
#' @return The elements of \code{x} surrounded by Regex word boundaries,
#' collapsed with logical ors, and surrounded by parantheses:
#' \code{(\\bx[1]\\b|\\bx[2]\\b|...)}.
.prep_pattern <- function(x) {
  # surround all elements with word boundaries, collapse with logical OR
  pattern <- paste0("\\b", x, "\\b", collapse = "|")

  # add parantheses to catch as group
  pattern <- paste0("(", pattern, ")")
  return(pattern)
}
