#' Precede greek letters with backslashes for LaTeX.
#'
#' Precede greek letters or whatever supplied in \code{alphabet} with
#' backslashes, such that when \code{cat()} is applied, the output can be
#' copied into a \code{.tex} document and be rendered.
#'
#' @param string A character vector.
#' @param alphabet A character vector of strings to be preceded by backslashes.
#' @param append Boolean, should the supplied \code{alphabet} be appended to the
#' implemented greek letters.
#' @return \code{string} with all greek letters, and what is supplied in
#' \code{alphabet} preceded by backslashes.
#' @examples
#' latex_greeks("alpha and beta and foo will be modified", "foo", TRUE)
latex_greeks <- function(string, alphabet = NULL, append = FALSE) {
  if(is.null(alphabet) & append) stop("Supply an alphabet to be appended.")

  greeks <- c(
    "[aA]lpha", "[bB]eta", "[gG]amma", "[dD]elta", "[eE]psilon", "[zZ]eta",
    "[eE]ta", "[tT]heta", "[iI]ota", "[kK]appa", "[lL]ambda", "[mM]u", "[nN]u",
    "[oO]micron", "[pP]i", "[rR]ho", "[sS]igma", "[tT]au", "[uU]psilon",
    "[pP]hi", "[cC]hi", "[pP]si", "[oO]mega"
  )

  if(is.null(alphabet)) {
    alphabet <- greeks
  } else if(append) {
    alphabet <- c(greeks, alphabet)
  }

  pattern <- .prep_pattern(alphabet)

  # \\\\ to produce single backslash with cat(), proceded by \\1 (group 1)
  replacement <- "\\\\\\1"

  res <- gsub(pattern, replacement, string)
  return(res)
}
