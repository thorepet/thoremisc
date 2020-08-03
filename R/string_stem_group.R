#' Stem all words in a group.
#'
#' Take a character vector of groups of words, and stem all words in each group.
#' The results are collapsed back into the original grouping, with words
#' separated by a single whitespace. This function wraps
#' \code{SnowballC::wordStem()} which can only handle single words as input.
#'
#' Designed to take cleaned strings (see \code{string_clean()}) as input.
#'
#' @param string A character vector containing strings of 1 or more words.
#' @param language Passed to \code{SnowballC::wordStem()}, see
#'   \code{SnowballC::getStemLanguages()}.
#' @return \code{string}, with all individual words stemmed, collapsed back into
#'   the original groups of words
#' @examples
#' string_stem_group(c("The coolest words", "Will all be stemmed"))
#' @export
string_stem_group <- function(string, language) {
  # split input along whitespaces
  string <- str_split(string, "\\s")

  # stem all words
  string <- lapply(string, SnowballC::wordStem, language = language)

  # collapse stemmed words back into word groups
  string <- sapply(string, paste, collapse = " ")

  string <- string_redund_ws(string)

  return(string)
}
