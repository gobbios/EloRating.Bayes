% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_seq.R
\name{prep_seq}
\alias{prep_seq}
\alias{prep_seq_sport}
\title{prepare interaction data}
\usage{
prep_seq(
  winner,
  loser,
  Date,
  presence = NULL,
  draws = NULL,
  intensity = NULL,
  estimate_startspread = FALSE
)

prep_seq_sport(
  winner,
  loser,
  Date,
  presence = NULL,
  draws = NULL,
  home_team = NULL,
  away_team = NULL,
  home_win = NULL
)
}
\arguments{
\item{winner, loser}{character vector with ids of winner and loser individuals}

\item{Date}{Date or character vector with dates (\code{"YYYY-MM-DD"})}

\item{presence}{presence (one line per interaction, which is different from
the approach in the classic EloRating package!!!)}

\item{draws}{optional logical or integer (0/1) vector with information
about draws (undecided/ties)}

\item{intensity}{optional character or factor describing type or intensity
for each interaction. Determines how many k values are estimated.
At its default (\code{NULL}), all interactions are considered of
the same type/intensity and one k value is estimated.}

\item{estimate_startspread}{logical, default is \code{FALSE}. Should the SD
of the start ratings be estimated. At its default, the SD is set to
1. When the SD is estimated, its prior is \code{exponential(1);}.}

\item{home_team, away_team, home_win}{for sports model}
}
\value{
a list
}
\description{
then to be used with \code{\link{elo_seq_bayes}}
}
\details{
see \code{\link{elo_seq_bayes}} for examples
}
