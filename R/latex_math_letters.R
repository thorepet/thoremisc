#' Toggle LaTeX math mode for leading letters.
#'
#' Wrap the leading letters in \emph{t}-test, \emph{F}-statistic,
#' \emph{p}-value, etc. in $ to toggle LaTeX math mode. Any combination to
#' leading letter and trailing word will also be modified (i.e. if \emph{t}-test
#' and \emph{F}-statistic are given, \emph{t}-statistic and \emph{F}-test will
#' also be matched).
#'
#' @param string A character vector.
#' @param convert A character vector of hyphenated words to wrap.
#' @param append Boolean, should the words in \code{convert} be appended to the
#'   implemented words.
#' @return \code{string} with leading letters wrapped in \code{$}.
#' @examples
#' latex_math_letters("The t-test and z-score.", "z-score")
#' @export
latex_math_letters <- function(string, convert = NULL, append = TRUE) {
  # baseline leading letters
  lead_letters <- c("tpF")

  # baseline trailing words
  words <- c("test", "statistic", "value")

  if(!is.null(convert)) {
    # split more words, returns list of length(convert)
    s <- strsplit(convert, "-", fixed = TRUE)

    # grab first part of split strings, collapse into atomic character
    add_letters <- paste0(sapply(s, `[`, 1), collapse = "")

    # grab second part of split strings
    add_words <- sapply(s, `[`, 2)

    if(append) {
      lead_letters <- paste0(lead_letters, add_letters, collapse = "")
      words <- c(words, add_words)
    } else if(!append) {
      lead_letters <- add_letters
      words <- add_words
    }
  }

  # match first letter of the words, replace with class of that letter either
  # upper- or lowercase, must set perl = TRUE to allow replacing with groups
  words_case <- gsub("^(\\w)", "[\\L\\1\\U\\1]", words, perl = TRUE)

  # precede all words with "-", collapse with logical OR
  words_regex <- paste0(paste0("-", words_case), collapse = "|")

  # create pattern to match group of class of leading letters followed by
  # lookahead of trailing words
  pattern <- paste0("([", lead_letters, "])(?=", words_regex, ")")
  replacement <- "$\\1$"

  res <- gsub(pattern, replacement, string, perl = TRUE)
  return(res)
}
