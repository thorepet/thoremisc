#' Move the caption to the bottom of the table produced by
#' \code{stargazer::stargazer()}.
#'
#' Credit to https://stackoverflow.com/a/42681368
#'
#' @param string An atomic character produced by \code{stargazer::stargazer()}.
#' @param to_console Boolean, should the output be returned, or \code{cat()} to
#' the console (and invisibly returned)?
#' @return \code{x} with the caption moved to the bottom.
#' @examples
#' df <- data.frame(a = rnorm(5), b = rnorm(5))
#' sg <- stargazer::stargazer(df)
#' latex_caption_to_bottom(sg, FALSE)
#' @export
latex_caption_to_bottom <- function(string, to_console = FALSE) {
  cap <- grep("\\\\caption", string)
  lab <- grep("\\\\label", string)
  last <- grep("\\\\end\\{table", string)

  res <- paste(
    c(string[-last], string[cap], string[lab], string[last])[-c(cap, lab)],
    collapse = "\n"
  )

  if(to_console) {
    cat(res, "\n")
    invisible(res)
  } else {
    return(res)
  }
}

#' Add a caption to a \code{stargazer::stargazer()} output.
#'
#' @param string An atomic character produced by \code{stargazer::stargazer()}.
#' @param caption An atomic character to be added as the caption.
#' @param to_bottom Boolean, should the caption moved to the bottom?
#' @return \code{string} with an added \code{caption}
#' @examples
#' df <- data.frame(a = rnorm(5), b = rnorm(5))
#' sg <- stargazer::stargazer(df)
#' latex_add_caption(sg, "This is going to be the caption", TRUE)
#' @export
latex_add_caption <- function(string, caption, to_bottom = TRUE) {
  if(to_bottom) string <- latex_caption_to_bottom(string, to_console = FALSE)

  pattern <- "caption[{][}]"
  replacement <- paste0("caption{", caption, "}")
  res <- gsub(pattern, replacement, string)
  return(res)
}
