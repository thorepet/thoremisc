% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pan_regularize.R
\name{pan_regularise}
\alias{pan_regularise}
\title{Regularise panel data.}
\usage{
pan_regularise(x, g, t)
}
\arguments{
\item{x}{A \code{data.frame} or \code{data.table}.}

\item{g}{An atomic character, name of the panel group variable in \code{x}.}

\item{t}{An atomic character, name of the panel time variable in \code{x}.}
}
\value{
A \code{data.frame} or \code{data.table} depending on the input.
}
\description{
Regularise a panel with non-existing rows or timesteps within the series. The
panel will be extend per group to the longest temporal coverage across groups
to make a balanced panel (at least in the group and time variables). The
series will be padded with \code{NA}s.
}
\details{
Currently supported are yearly, quaterly, and monthly data.
}
\examples{
x <- data.frame(
 yr = rep(2000:2002, 2),
 grp = rep(c("A", "B"), each = 3),
 val = 1:6
 )
x <- x[-2, ]
x_reg <- pan_regularise(x, "grp", "yr")
}
