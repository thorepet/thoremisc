#' Wrap abbreviations in text in a LaTeX command.
#'
#' @param string A character vector.
#' @param acros A character vector of abbreviations to wrap.
#' @param command A character (without the preceding backslash).
#' @return \code{string} with all \code{acros} wrapped in \code{command}.
#' @examples
#' latex_acro("This package is not on CRAN.", "CRAN")
#' @export
latex_acro <- function(string, acros, command = "acs*") {
  pattern <- .prep_pattern(acros)
  replacement <- paste0("\\\\", command, "{\\1}")
  res <- gsub(pattern, replacement, string)
  return(res)
}
