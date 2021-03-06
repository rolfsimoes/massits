% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/its.features.R
\name{its.samples.feat}
\alias{its.samples.feat}
\title{massits features functions}
\usage{
its.samples.feat(m, bands = its.bands())
}
\arguments{
\item{m}{A valid massits tibble}

\item{bands}{A valid string vector of band names. These names must coincide with
\code{m} columns (Default \code{its.bands()}).}
}
\value{
Massits feature tibble
}
\description{
Transforms a massits tibble as an massits feature tibble
}
\author{
Rolf Simoes, \email{rolf.simoes@inpe.br}
}
