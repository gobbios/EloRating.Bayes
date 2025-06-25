#' index position for extraction dates
#'
#' @param idates character or Date vector with interaction
#'        dates (of the observed interctions)
#' @param edates character or Date vector of dates for which corresponding
#'        ratings are to be extracted. Default is \code{NULL}, i.e.,
#'        extract ratings after first interaction and ratings after
#'        last interaction date.
#' @param standat stan data list
#' @param id vector with ids
#'
#' @returns a named integer vector
#' @export
#'
#' @examples
#' idates <- c("2000-01-04", "2000-03-27", "2000-04-01", "2000-04-01", "2000-04-24")
#' find_extract_index(idates)
#' # return index of first date (with message)
#' find_extract_index(idates, edates = "1999-01-01")
#' find_extract_index(idates, edates = c("2000-01-09", "2000-04-15"))
#' find_extract_index(idates, edates = c("2000-04-09", "2020-04-15"))
#'
#' data(adv, package = "EloRating")
#' standat <- prep_seq(winner = adv$winner, loser = adv$loser, Date = adv$Date)
#' find_extract_index2(standat, c("a", "b"))

find_extract_index <- function(idates, edates = NULL) {
  if (any(order(unique(idates)) != seq_along(unique(idates)))) {
    stop("dates are not in order")
  }

  if (is.null(edates)) {
    out <- c(1, length(idates))
    names(out) <- c(idates[1], idates[length(idates)])
    return(out)
  }

  edates <- sort(edates)
  if (any(edates < idates[1])) {
    message("dates found that were recorded *before* first interaction")
    edates[edates < idates[1]] <- as.character(idates[1])
  }

  sapply(edates, \(x)max(which(idates <= x)))
}

#' @export
find_extract_index2 <- function(standat, id) {
  x="a"
  out <- lapply(id, \(x) {
    xl <- which(names(standat$winner_index) == x | names(standat$loser_index) == x)
    data.frame(id = x,
               date = names(standat$idates)[xl],
               win = names(standat$winner_index[xl]) == x,
               draw_row = xl,
               draw_col = which(colnames(standat$presence) == x))
  })
  do.call("rbind", out)
}

