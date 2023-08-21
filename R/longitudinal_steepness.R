
#' steepness at specific dates
#'
#' @param res result of \code{\link{elo_seq_bayes}}
#' @param dates character of target date(s) (YYYY-MM-DD)
#'
#' @return a vector with one column for each entry in \code{dates}
#' @export
#'
#' @examples
#' # 3 periods with varying steepness:
#' x <- generate_interactions(n_ind = 12, n_int = c(200, 200, 200), steep = c(0.9, 0.2, 0.6))
#' res <- elo_seq_bayes(prep_seq(winner = x$winner, loser = x$loser, Date = x$date), parallel_chains = 4)
#' dates <- c(x$date[max(which(x$set == 1))], x$date[max(which(x$set == 2))], x$date[max(which(x$set == 3))])
#' r <- longitudinal_steepness(res = res, dates = dates)
#' colMeans(r)


longitudinal_steepness <- function(res, dates) {

  # helper function...
  steep_short <- function(vec) {
    aux <- rowSums(sapply(seq_along(vec), function(x) 1 / (1 + exp(vec[x] - vec)))) - 0.5
    coef(lm(sort(aux) ~ seq_along(vec)))[2]
  }

  e <- extract_elo_b(res = res, make_summary = FALSE, targetdate = dates, keep_absent = FALSE)
  outres <- matrix(ncol = length(dates), nrow = nrow(e[[1]]))

  for (i in seq_along(dates)) {
    ratings_temp <- e[[i]]
    outres[, i] <- temp_steep <- apply(ratings_temp, 1, steep_short)
  }
  outres
}
