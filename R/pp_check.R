#' posterior predictive checks
#'
#' by individual
#'
#' @param res result from \code{\link{elo_seq_bayes}}
#' @param n_samples number of simulated draws to plot
#' @importFrom graphics box mtext
#' @return a plot
#' @export
#'
#' @details
#' Red is the obseverd winning proportion of each individual.
#'
#' Summaries of simulated draws are added in black (cross = median, line = 80% range)
#'
#'
#' @examples
#' res <- toydata()
#' pp_check(res, n_samples = 10)

pp_check <- function(res, n_samples = 50) {
  postmat <- res$mod_res$draws("gq_winprobs", format = "draws_matrix")
  # postmat <- res$mod_res$draws("gq_int_outcome", format = "draws_matrix")

  w_obs <- names(res$standat$winner_index)
  l_obs <- names(res$standat$loser_index)
  o_mat <- cbind(w_obs, l_obs)
  u <- unique(c(w_obs, l_obs))
  w <- factor(w_obs, levels = u)
  l <- factor(l_obs, levels = u)

  # props per individual
  props_observed <- table(w) / (table(w) + table(l))

  # results container
  prop_mat <- matrix(ncol = length(props_observed), nrow = n_samples)
  colnames(prop_mat) <- names(props_observed)
  # from posterior winprobs
  for (i in seq_len(n_samples)) {
    prob_row <- c(postmat[sample(nrow(postmat), 1), ])
    wins <- rbinom(length(prob_row), 1, prob_row)
    simres <- t(sapply(seq_along(wins), function(x) {
      if (wins[x] == 1) {
        o_mat[x, ]
      } else {
        rev(o_mat[x, ])
      }
    }))
    w <- factor(simres[, 1], levels = u)
    l <- factor(simres[, 2], levels = u)

    # props per individual
    prop_mat[i, ] <- table(w) / (table(w) + table(l))

  }


  plot(0, 0, ylim = c(0, 1), xlim = c(0, length(props_observed) + 1),
       type = "n", axes = FALSE, xlab = "", ylab = "proportion won"
       )
  axis(2, las = 1)
  axis(1, tcl = 0, labels = names(props_observed), at = seq_len(ncol(prop_mat)))
  points(seq_len(ncol(prop_mat)), props_observed, pch = 16, col = "red", cex = 1.5)
  points(seq_len(ncol(prop_mat)), apply(prop_mat, 2, median), pch = 4)
  segments(seq_len(ncol(prop_mat)), apply(prop_mat, 2, quantile, 0.1),
           seq_len(ncol(prop_mat)), apply(prop_mat, 2, quantile, 0.9))
  mtext(table(w) + table(l), 3, line = 0.5, at = seq_len(ncol(prop_mat)), cex = 0.7)
  box()

  invisible(list(obs = props_observed, sim = prop_mat))
}



