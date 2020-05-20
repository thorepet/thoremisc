.onAttach <- function(libname, pkgname) {

  t <- try(
    expr = {
      p <- xml2::read_html("https://onelinefun.com/one-liner-of-the-day/")
      n <- rvest::html_node(p, xpath = "/html/body/div[1]/article/div[1]/p")
      t <- rvest::html_text(n)
      packageStartupMessage(paste("Today's joke:", t, sep = "\n"))
    }
  )

  if(class(t) %in% "try-error") {
    packageStartupMessage("No joke today.")
  }

#   packageStartupMessage(
#   "Thore's miscellaneous functions ready to work their magic"
#   )
}
