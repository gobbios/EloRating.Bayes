
#' generate dominance interaction sequence with varying steepness
#'
#' steepness changes are induced via periods determined by interaction numbers
#'
#' @param n_ind integer, number of individuals (default is 10)
#' @param n_int integer vector, number of interactions per steepness period
#' @param steep numeric vector, steepness per period (needs to be the same
#'              length as \code{n_int})
#' @param shuffle_dates logical, create regular date sequence or shuffle dates,
#'                      such that some dates are repeated and others don't
#'                      occur
#'
#' @return a data frame with interaction sequence
#' @export
#'
#' @examples
#' # 15 interactions with steepness 0.9
#' # followed by 10 interactions with steepness 0.3
#' generate_interactions(n_ind = 6, n_int = c(15, 10), steep = c(0.9, 0.3))
#'

generate_interactions <- function(n_ind = 10,
                                  n_int = c(100, 100),
                                  steep = c(0.9, 0.2),
                                  shuffle_dates = FALSE
                                  ) {
  dyads <- t(combn(seq_len(n_ind), 2))
  out <- matrix(ncol = 2, nrow = 0)

  for (i in seq_along(n_int)) {
    s <- matrix(ncol = 2, nrow = n_int[i])
    for (k in seq_len(n_int[i])) {
      s[k, ] <- dyads[sample(nrow(dyads), 1), ]
      if (runif(1, 0, 1) > (steep[i] + 1)/2) {
        s[k, ] <- rev(s[k, ])
      }
    }
    out <- rbind(out, s)
  }

  d <- seq.Date(from = as.Date("2000-01-01"), length.out = nrow(out), by = "day")
  if (shuffle_dates) {
    d <- sort(sample(d, nrow(out), replace = TRUE))
  }

  data.frame(winner = out[, 1], loser = out[, 2], date = d, set = rep(seq_along(n_int), times = n_int))
}
