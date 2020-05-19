#' Write \code{string} to disk.
#'
#' Designed for a \code{string} produced by \code{stargazer::stargazer()}. If
#' supplied, \code{comment} will be added as a LaTeX comment in the first line
#' of \code{string}. \code{filename} may or may not end in \code{.tex}, it will
#' be corrected.
#'
#' @param string A character vector.
#' @param filename An atomic character.
#' @param comment An atomic character.
#' @return empty.
#' @examples
#' latex_to_disk("This is a \\latex document.", "example.tex")
#' @export
latex_to_disk <- function(string, filename, comment = NULL) {
  # extract potential file extension
  file_ext <- regmatches(
    filename,
    regexpr("(?<=[.])[a-z]{1,}$", filename, perl = TRUE)
  )
  if(length(file_ext) > 0) {
    if(file_ext != "tex") warning(
      paste("Are you sure the file ending is supposed to be", file_ext, "?")
    )
  } else {
    filename <- paste0(filename, ".tex")
  }

  # # alternative to if() statements above
  # if(!grepl("[.]tex$", filename)) filename <- paste0(filename, ".tex")

  if(!is.null(comment)) {
    string <- paste(paste("%", comment), string, sep = "\n")
  }

  cat(string, file = filename)
}
