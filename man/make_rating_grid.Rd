% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/principled_interactions.R
\name{make_rating_grid}
\alias{make_rating_grid}
\title{create rating matrix per day}
\usage{
make_rating_grid(n_ind = 10, n_days = 100, start_sd = 1, drifts = 0)
}
\arguments{
\item{n_ind}{numeric, number of individuals (defaul is \code{10})}

\item{n_days}{numeric, number of days (defaul is \code{100})}

\item{start_sd}{positive numeric, SD of start ratings (defaul is \code{1})}

\item{drifts}{numeric, the number of 'rank change' events
(default is \code{0}). When this is 0, the system is temporarilly
stable.}
}
\value{
a list
}
\description{
as basis for simulating interactions
}
\details{
if start_sd = 0: internally, this will be change to 0.000001, so that no
winprob of exactly 50\% can occur. Practically, this doesn't make a
difference except for the expectation column.
}
\examples{
lapply(make_rating_grid(7, 100, drifts = 1), head, 3)
}
