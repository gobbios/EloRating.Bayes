#' prepare interaction data
#'
#' then to be used with \code{\link{elo_seq_bayes}}
#'
#' @param winner character
#' @param loser character
#' @param Date Date
#' @param presence presence (one line per interaction, which is different from
#'                 the approach in the classic EloRating package!!!)
#' @param draws optional logical or integer (0/1) vector with information
#'              about draws (undecided/ties)
#'
#' @importFrom EloRating seqcheck
#' @return a list
#' @details see \code{\link{elo_seq_bayes}} for examples
#' @export


# x <- randomsequence(nID = 10, avgIA = 50, presence = c(0.2, 0.4))
# winner <- x$seqdat$winner
# loser <- x$seqdat$loser
# Date <- x$seqdat$Date
# presence <- x$pres

prep_seq <- function(winner, loser, Date, presence = NULL, draws = NULL) {
  if (is.factor(winner)) winner <- as.character(winner)
  if (is.factor(loser)) loser <- as.character(loser)
  if (is.numeric(winner) | is.numeric(loser)) {
    winner <- as.character(winner)
    loser <- as.character(loser)
    warning("winner and/or loser vectors were detected as being numeric. This hasn't been tested, but should (should!) work. Consider providing these data as character or factor.")
  }

  if (is.null(draws)) {
    draws <- rep(0, length(winner))
  } else {
    draws <- as.integer(draws)
  }

  x <- seqcheck(winner, loser, Date)

  all_ids <- unique(c(winner, loser))
  winner_index <- as.numeric(sapply(winner, function(x)which(all_ids == x)))
  loser_index <- as.numeric(sapply(loser, function(x)which(all_ids == x)))
  names(winner_index) <- winner
  names(loser_index) <- loser
  n_ind <- length(all_ids)
  n_int <- length(winner_index)

  if (is.null(presence)) {
    presence <- matrix(ncol = n_ind, nrow = n_int, 1)
    colnames(presence) <- all_ids
  } else {
    presence <- as.matrix(presence[, all_ids])
  }

  # make dates to be included in standata
  # workaround using a named vector of zeros
  xdates <- rep(0, n_int)
  names(xdates) <- as.character(as.Date(Date))

  standat <- list(n_ind = n_ind,
                  n_int = n_int,
                  winner_index = winner_index,
                  loser_index = loser_index,
                  draws = draws,
                  presence = presence,
                  idates = xdates,
                  targetdates = c(1, n_int),
                  n_extract = 2)
  standat
}

