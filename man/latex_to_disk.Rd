% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latex_to_disk.R
\name{latex_to_disk}
\alias{latex_to_disk}
\title{Write \code{string} to disk.}
\usage{
latex_to_disk(string, filename, comment = NULL)
}
\arguments{
\item{string}{A character vector.}

\item{filename}{An atomic character.}

\item{comment}{An atomic character.}
}
\value{
empty.
}
\description{
Designed for a \code{string} produced by \code{stargazer::stargazer()}. If
supplied, \code{comment} will be added as a LaTeX comment in the first line
of \code{string}. \code{filename} may or may not end in \code{.tex}, it will
be corrected.
}
\examples{
latex_to_disk("This is a \\\\latex document.", "example.tex")
}
