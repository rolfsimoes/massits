% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/its.utils.R
\name{its.feat.valid}
\alias{its.feat.valid}
\title{massits utils functions}
\usage{
its.feat.valid(f, err_desc = NULL)
}
\arguments{
\item{f}{A valid massits features tibble}

\item{err_desc}{An error description to be returned.}
}
\value{
Logical
}
\description{
Tests if \code{f} is a valid massits feature data.
              Returns \code{TRUE} if the input is valid. Else, returns \code{FALSE} or
              throws an exception if \code{err_desc} is informed.
}
\author{
Rolf Simoes, \email{rolf.simoes@inpe.br}
}
