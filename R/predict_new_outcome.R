#' predict outcome
#'
#' @param model result from \code{\link{elo_seq_bayes}}
#' @param i1,i2 two individuals for which to get the outcome
#' @param threshold default is 0.6: how certain are we about the outcome reaching this level of confidence
#'
#' @return a data frame and a plot
#' @export
#'
#' @examples
#' data(adv, package = "EloRating")
#' standat <- prep_seq(winner = adv$winner, loser = adv$loser, Date = adv$Date)
#' res <- elo_seq_bayes(standat = standat, refresh = 0, chains = 3, iter_sampling = 303)
#' extract_elo_b(res)
#' # clear expectation for c vs g
#' predict_new_outcome(res, "c", "g")
#' predict_new_outcome(res, "g", "c")
#' # less clear expectation for d vs f
#' predict_new_outcome(res, "d", "f")
#' # no clear expectation between e and f
#' predict_new_outcome(res, "f", "e")

# i="c";i2="g"
predict_new_outcome <- function(model, i1, i2, threshold = 0.8) {
  p <- model$mod_res$draws("out_perdate", format = "draws_matrix")
  p <- p[, grepl("[2,", colnames(p), fixed = TRUE)]
  colnames(p) <- colnames(model$standat$presence)

  p <- p[, c(i2, i1)]
  p <- 1 / (1 + exp(p[, 1] - p[, 2]))

  # plot
  d <- density(p)
  ymax <- max(d$y)
  plot(0, 0, "n", xlim = c(0, 1), xaxs = "i", ylim = c(0, ymax * 1.1), yaxs = "i", axes = FALSE, ann = FALSE)
  polygon(d, col = "#009634")
  axis(1, at = c(0:4)/4, labels = c(1, 0.75, 0.5, 0.75, 1))
  arrows(x0 = 0.25, y0 = ymax * 1.05, x1 = 0.75, y1 = ymax * 1.05, code = 3)
  text(0.15, ymax * 1.05, paste(i2, "wins"))
  text(0.85, ymax * 1.05, paste(i1, "wins"))
  abline(v = 0.5, lty = 2)

  out <- data.frame(i = c(i1, i2), post_win_prob = c(mean(p), mean(1 - p)))
  # sim <- rbinom(nrow(p), 1, p)

  if (out$post_win_prob[1] > out$post_win_prob[2]) {
    threshprob <- mean(p > threshold)
    winner <- c(TRUE, FALSE)
  } else {
    threshprob <- mean(p < (1 - threshold))
    winner <- c(FALSE, TRUE)
  }
  out$winner <- winner
  out$threshprob <- NA
  out$threshprob[out$winner] <- threshprob
  out
}

