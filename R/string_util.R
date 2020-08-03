#' Utility functions for string processing.
#'
#' Replace any characters that do not belong to Regex classes \\w or \\d, or are
#' a literal whitespace, by a single whitespace. The function preserves German
#' Umlaute and diacritical letters.
#'
#' Elaboration on the Regex classes:
#' https://stackoverflow.com/a/2998550/13542638.
#'
#' @param string A character vector.
#' @return \code{string} with special Unicode characters replaced by a
#'   whitespace.
#' @examples
#' thoremisc:::.remove_special_chars("The following will be modified: hello-world.")
#' @importFrom stringr str_replace_all
.remove_special_chars <- function(string) {
  string <- str_replace_all(string, "[^\\w\\d ]", " ")
  return(string)
}

#' Utility functions for string processing.
#'
#' Replace German Umlaute by their ASCII representations: "ä"->"ae", "ö"->"oe",
#' and "ü"->"ue". "ß" is diacritical, see \code{.remove_diacritics()}.
#'
#' @param string A character vector.
#' @return \code{string} with German Umlaute replaced.
#' @examples
#' thoremisc:::.replace_umlaute("Äh, trörö in Überlingen, nicht auf dem Darß.")
#' @importFrom stringr str_replace_all
.replace_umlaute <- function(string) {
  df <- data.frame(
    umlaut = c("ä", "ö", "ü"),
    replace = c("ae", "oe", "ue")
  )

  for(i in 1:nrow(df)) {
    string <- str_replace_all(string, df$umlaut[i], df$replace[i])
  }

  return(string)
}

#' Utility functions for string processing.
#'
#' Replace diacritical letters(é, ç, ...) with their "plain" versions. This
#' function can only handle diacritical letters from latin alphabets. Elements
#' in \code{string} containinig non-latin letters (e.g. cyrillic), will be
#' replaced by \code{NA}.
#'
#' Reference: https://stackoverflow.com/a/20495866/13542638
#'
#' @param string A character vector.
#' @return \code{string} with diacritical letters replaced by their ASCII
#'   versions.
#' @examples
#' thoremisc:::.remove_diacritics("Åll thëşé fūñny leŧters wîll be nørmalised.")
#' @importFrom stringr str_detect str_remove_all
.remove_diacritics <- function(string) {
  if(any(str_detect(string, "[äöü]"), na.rm = TRUE)) warning("Umlaute dropped.")

  # transliterate to ASCII: á->'a, è->`e, ë->"e, â->^a, æ->ae, ç->c, å->a, ...
  string <- iconv(string, to = "ASCII//TRANSLIT")

  # remove "'", "`", "^", and """
  string <- str_remove_all(string, "['`\\^\"~]")

  if(any(is.na(string))) warning("Some strings could not be transliterated.")

  return(string)
}

#' Remove redundant whitespaces from a string.
#'
#' Replace consecutive whitespaces by a single one, and trim leading and
#' trailing whitespace.
#'
#' @param string A character vector.
#' @return \code{string} with redundant whitespaces removed.
#' @examples
#' string_redund_ws(" This  string   will be shorter.   ")
#' @export
#' @importFrom stringr str_replace_all
string_redund_ws <- function(string) {
  string <- str_replace_all(string, "\\s+", " ")
  string <- trimws(string)

  return(string)
}
