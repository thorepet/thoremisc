#' Clean letters in a string.
#'
#' Transform \code{string} into a simple lowercase ASCII representation. Options
#' to convert \code{string} to lowercase, remove non-letter characters, replace
#' German Umlaute, and transliterate diacritical letters (á, å, ê, ...).
#'
#' The function removes full stops, i.e. breaks sentences.
#'
#' @param string A character vector.
#' @param lower Boolean, convert \code{string} to lowercase.
#' @param spec_chars Boolean, remove non-letters.
#' @param umlaute Boolean, replace German Umlaute.
#' @param diacritics Boolean, transliterate diacritical letters.
#' @return \code{string}, modified.
#' @examples
#' string_clean("Thîs sŧriñg will bé å løt simplêr. Köln is not in M-V.")
#' @seealso The function internally calls \code{\link{.remove_special_chars}},
#'   \code{\link{.replace_umlaute}}, and \code{\link{.remove_diacritics}}.
#' @export
string_clean <- function(
  string, lower = TRUE, spec_chars = TRUE, umlaute = TRUE, diacritics = TRUE
) {
  # The order matters. German Umlaute are diacritics, and are converted into
  # their "base" letters by remove_diacritics() (ä->a). To preserve them, use
  # replace_umlaute() first (ä->ae).

  if(lower) string <- tolower(string)
  if(spec_chars) string <- .remove_special_chars(string)
  if(umlaute) string <- .replace_umlaute(string)
  if(diacritics) string <- .remove_diacritics(string)

  string <- string_redund_ws(string)

  return(string)
}
