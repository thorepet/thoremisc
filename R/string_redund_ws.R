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
