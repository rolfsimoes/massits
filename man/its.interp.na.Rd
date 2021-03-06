% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/its.interp.R
\name{its.interp.na}
\alias{its.interp.na}
\title{massits interpolation functions}
\usage{
its.interp.na(m, interp = c("spline", "last", "linear"),
  bands = its.bands())
}
\arguments{
\item{m}{A valid massits tibble}

\item{interp}{A string indicating what interpolation to use.
Can assume one of the following values:
\code{"last"}, \code{"linear"}, and \code{"spline"}
(Default \code{"spline"}).}

\item{bands}{A string vector indicating those fields (bands)
to be interpolated. All non selected fields
will remain unchanged (Default \code{its.bands()}).}
}
\value{
Massits table
}
\description{
Performs an interpolation on those measurements with NA value in
              some massits tibble's columns.
}
\author{
Rolf Simoes, \email{rolf.simoes@inpe.br}
}
