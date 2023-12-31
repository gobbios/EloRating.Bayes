% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_helper.R
\name{date2index}
\alias{date2index}
\title{find index for extraction of rating(s) using dates}
\usage{
date2index(dateseq, targetdate)
}
\arguments{
\item{dateseq}{vector of dates (dates of observed interactions)}

\item{targetdate}{vector of dates (date(s) for which posteriors are requested)}
}
\value{
an integer vector of index positions
}
\description{
this is a helper function to convert calender dates into index positions
}
\examples{
td <- as.Date("2000-01-15")
d <- as.Date(c("2000-01-10", "2000-01-13", "2000-01-16"))
date2index(d, td)

d <- as.Date(c("2000-01-10", "2000-01-13", "2000-01-15", "2000-01-15", "2000-01-16"))
date2index(d, td)

d <- as.Date(c("2000-01-15", "2000-01-16"))
date2index(d, td)

# error because target date is outside date range
d <- as.Date(c("2000-01-10", "2000-01-11"))
\dontrun{
date2index(d, td)
}

}
