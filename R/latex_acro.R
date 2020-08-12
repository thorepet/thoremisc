#' Wrap abbreviations within a \code{string} in a LaTeX command.
#'
#' Wrap abbreviations in a given LaTeX command to toggle the \code{acronym} or
#' \code{glossary} packages. \code{command} defaults to the \code{acronym}
#' package's short command, that does not mark abbreviations as used (by
#' including "*"). This avoids the acronym being rendered the first time in the
#' list of figures or tables.
#'
#' @param string A character vector.
#' @param acros A character vector of abbreviations to wrap.
#' @param command A character (without the preceding backslash).
#' @return \code{string} with all \code{acros} wrapped in \code{command}.
#' @examples
#' latex_acro("This package is not on cran.", "cran")
#' @export
latex_acro <- function(string, acros, command = "acs*") {
  pattern <- .prep_pattern(acros)
  replacement <- paste0("\\\\", command, "{\\1}")
  res <- gsub(pattern, replacement, string)
  return(res)
}
