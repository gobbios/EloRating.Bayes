#' generate interaction starting from actual ratings
#'
#' for now, we assume a stable system, i.e. individuals don't change ratings
#' (i.e. effectively k=0)
#'
#' @param n_ind numeric, number of individuals
#' @param n_int numeric, number of interactions
#' @param startspread numeric, non-negative, SD of start ratings
#'
#' @details
#' if startspread = 0: internally, this will be change to 0.000001, so that no
#'   winprob of exactly 50% can occur. Practically, this doesn't make a
#'   difference except for the expectation column.
#'
#'
#' @returns a list with two items. First a data frame with interactions and
#'   second a vector with the start ratings.
#' @export
#'
#' @examples
#' principled_interactions(n_ind = 6, n_int = 20, startspread = 5)
#' # most outcomes follow expectation:
#' table(principled_interactions(n_ind = 6, n_int = 1000, startspread = 5)[[1]]$expected)
#'
#' # random outcomes with equal start ratings for everyone
#' principled_interactions(n_ind = 6, n_int = 20, startspread = 0)
#' table(principled_interactions(n_ind = 6, n_int = 1000, startspread = 0)[[1]]$expected)
principled_interactions <- function(n_ind = 10,
                                    n_int = 100,
                                    startspread = 1)
  {
  # that's the function that gives the winprob for the first (r1)
  # it's the one in use in the Stan functions
  winprob <- function(r1, r2) {
    1/(1 + exp((r2 - r1)));
  }
  if (startspread == 0) startspread <- 1e-06
  startratings <- as.numeric(scale(rnorm(n_ind))) * startspread


  out <- t(sapply(seq_len(n_int), \(x)sample(seq_len(n_ind), 2, replace = FALSE)))

  i=1
  for (i in seq_len(n_int)) {
    wp <- winprob(startratings[out[i, 1]], startratings[out[i, 2]])
    r <- rbinom(1, 1, wp)
    if (r == 0) out[i, ] <- rev(out[i, ])
  }
  expected <- startratings[out[, 1]] > startratings[out[, 2]]
  # table(expected)


  out <- data.frame(winner = out[, 1], loser = out[, 2])
  d <- seq.Date(as.Date("2000-01-01"), as.Date("2004-12-31"), by = "day")
  out$date <- as.character(sort(sample(d, n_int, TRUE)))
  out$expected <- expected

  if (n_ind <= 26) {
    out$winner <- letters[out$winner]
    out$loser <- letters[out$loser]
    names(startratings) <- letters[1:n_ind]
  }

  list(idata = out, startratings = startratings)
}
