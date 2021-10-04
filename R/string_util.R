#' String processing utility functions.
#'
#' Short utility functions to clean certain characteristics of strings. These
#' are combined in \code{\link{string_clean}}.
#'
#' Replace any characters that do not belong to Regex classes \\w or \\d, or are
#' a literal whitespace, by a single whitespace. The function preserves German
#' Umlaute and diacritical letters.
#'
#' Elaboration on the Regex classes:
#' https://stackoverflow.com/a/2998550/13542638.
#'
#' Replace German Umlaute by their ASCII representations: "ä"->"ae", "ö"->"oe",
#' and "ü"->"ue". "ß" is diacritical and handled by \code{.remove_diacritics}.
#'
#' Replace diacritical letters(é, ç, ...) with their "plain" versions. This
#' function can only handle diacritical letters from latin-based alphabets.
#' Elements in \code{string} containinig non-latin letters (e.g. cyrillic), will
#' be replaced by \code{NA} and a warning will be given.
#'
#' Reference: https://stackoverflow.com/a/20495866/13542638
#'
#' @param string A character vector.
#' @name string_utility
#' @return \code{.remove_special_chars} returns \code{string} with non-letter
#'   Unicode characters replaced by a whitespace.
#'
#'   \code{.replace_umlaute} returns \code{string} with any German Umlaute
#'   replaced.
#'
#'   \code{.remove_diacritics} returns \code{string} with diacritical letters
#'   replaced by their ASCII versions.
#' @examples
#' thoremisc:::.remove_special_chars("This will be modified: hello-world.")
#' thoremisc:::.replace_umlaute("Äh, trörö in Überlingen, nicht auf dem Darß.")
#' thoremisc:::.remove_diacritics("Åll thëşé fūñny leŧters wîll be nørmalised.")
#' @seealso \code{\link{string_clean}} and \code{\link{string_redund_ws}}
#' @keywords internal
NULL
#> NULL

#' @rdname string_utility
#' @importFrom stringr str_replace_all
.remove_special_chars <- function(string) {
  string <- str_replace_all(string, "[^\\w\\d ]", " ")
  return(string)
}

#' @rdname string_utility
#' @importFrom stringr str_replace_all
.replace_umlaute <- function(string) {
  df <- data.frame(
    umlaut = c("\\u00e4", "\\u00f6", "\\u00fc"),
    replace = c("ae", "oe", "ue")
  )

  for(i in 1:nrow(df)) {
    string <- str_replace_all(string, df$umlaut[i], df$replace[i])
  }

  return(string)
}

#' @rdname string_utility
#' @importFrom stringr str_detect str_remove_all
.remove_diacritics <- function(string) {
  # escaped aöü, see stringi::stri_escape_unicode()
  if(any(str_detect(string, "[\\u00e4\\u00f6\\u00fc]"), na.rm = TRUE)) {
    warning("Umlaute dropped.")
  }

  # transliterate to ASCII: á->'a, è->`e, ë->"e, â->^a, æ->ae, ç->c, å->a, ...
  tstring <- iconv(string, to = "ASCII//TRANSLIT")

  # remove "'", "`", "^", """, "~"
  tstring <- str_remove_all(tstring, "['`\\^\"~]")

  if(any(is.na(tstring))) {
    translit_err <- paste(string[is.na(tstring)], collapse = "\n")
    wmessage <- paste(
      "The following strings could not be transliterated:", translit_err,
      sep = "\n"
    )
    warning(wmessage)
  }

  return(tstring)
}
