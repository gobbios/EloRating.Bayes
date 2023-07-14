#' find index for extraction of rating(s) using dates
#'
#' this is a helper function to convert calender dates into index positions
#'
#' @param dateseq vector of dates (dates of observed interactions)
#' @param targetdate vector of dates (date(s) for which posteriors are requested)
#'
#' @return an integer vector of index positions
#' @export
#'
#' @examples
#' td <- as.Date("2000-01-15")
#' d <- as.Date(c("2000-01-10", "2000-01-13", "2000-01-16"))
#' date2index(d, td)
#'
#' d <- as.Date(c("2000-01-10", "2000-01-13", "2000-01-15", "2000-01-15", "2000-01-16"))
#' date2index(d, td)
#'
#' d <- as.Date(c("2000-01-15", "2000-01-16"))
#' date2index(d, td)
#'
#' # error because target date is outside date range
#' d <- as.Date(c("2000-01-10", "2000-01-11"))
#' \dontrun{
#' date2index(d, td)
#' }
#'

date2index <- function(dateseq, targetdate) {
  # find the index position in dateseq that corresponds to most recent from targetdate
  # e.g. targetdate = 2000-01-15 but dateseq = (2000-01-10, 2000-01-13, 2000-01-16)
  # then the result would be 2:
  #   the ratings at index=2 are the most recent from targetdate's perspective
  # if target date actually occurs in the date seq: take the position of the last occurrence

  # some checks first:
  if (any(targetdate < min(dateseq))) {
    stop("(at least one) targetdate lies before date sequence beginning", call. = FALSE)
  }
  if (any(targetdate > max(dateseq))) {
    stop("(at least one) targetdate lies after date sequence end", call. = FALSE)
  }

  xres <- lapply(targetdate, function(x) {
    if (x %in% dateseq) {
      return(max(which(dateseq == x)))
    } else {
      return(min(which(dateseq >= x)) - 1)
    }
  })

  unlist(xres)
}



#' find origin for as.Date
#'
#' @param xdate a \code{Date}
#'
#' @return character
#' @export
#'
#' @examples
#' find_origin(as.Date("2000-10-10"))
find_origin <- function(xdate) {
  aux1 <- as.numeric(xdate)
  if (as.character(xdate) == as.character(as.Date(aux1, origin = "1970-01-01"))) return("1970-01-01")
  if (as.character(xdate) == as.character(as.Date(aux1, origin = "1899-12-30"))) return("1899-12-30")
  if (as.character(xdate) == as.character(as.Date(aux1, origin = "1899-12-31"))) return("1899-12-31")
  if (as.character(xdate) == as.character(as.Date(aux1, origin = "1900-01-01"))) return("1900-01-01")
  NULL
}
