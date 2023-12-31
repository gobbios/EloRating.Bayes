% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_presence.R
\name{convert_presence}
\alias{convert_presence}
\title{convert date-based presence to interaction-based presence}
\usage{
convert_presence(presence, interaction_dates, date_col = "Date")
}
\arguments{
\item{presence}{a data.frame with presence/absence data, which
needs to include a date column (YYYY-MM-DD) as
indicated by \code{date_col=}}

\item{interaction_dates}{vector of dates or character (YYYY-MM-DD) on
which interactions were observed}

\item{date_col}{character, column name for the date column in
\code{presence}. Default is \code{"Date"}.}
}
\value{
a data frame with presence data
}
\description{
convert date-based presence to interaction-based presence
}
\examples{
data("advpres", package = "EloRating")
presence <- advpres
nrow(presence) # covering 33 days
# 15 interactions:
idates <- sort(sample(presence$Date[1:20], 15, replace = TRUE))
# convert
presence_new <- convert_presence(presence, idates)
nrow(presence_new) # 15 rows
presence_new
}
