#' caterpillar plots for elo model
#'
#' @param res object from \code{\link{elo_seq_bayes}}
#' @param id character vector with ids to include (default is \code{NULL}:
#'          plot a random sample of all individuals, as constrained by
#'          \code{mfrow})
#' @param include_k plot k parameter first (default is \code{TRUE})
#' @param mfrow layout of plot (default is \code{c(2, 2)})
#'
#' @importFrom graphics legend par
#'
#' @returns a plot
#' @export
#'
#' @examples
#' x <- generate_interactions(n_ind = 6, n_int = c(100), use_letters = TRUE)
#' x <- data.frame(x)
#' standat <- prep_seq(winner = x$winner, loser = x$loser, Date = x$date)
#' res <- elo_seq_bayes(standat = standat, refresh = 0, parallel_chains = 2)
#' par(mar = c(2, 2, 1, 1), las = 1)
#' # k + plus 3 individuals
#' caterpillar_elo(res)
#' # only k
#' caterpillar_elo(res, mfrow = c(1, 1))
#' # two specific individuals
#' caterpillar_elo(res, id = c(x$winner[1], x$loser[1]), include_k = FALSE, mfrow = c(1, 2))


# id = NULL
# include_k = TRUE
caterpillar_elo <- function(res, id = NULL, include_k = TRUE, mfrow = c(2, 2)) {
  opar <- par(no.readonly = TRUE)
  cids <- chaind_ids(res$mod_res)
  pdata <- res$mod_res$draws(variables = "EloStart", format = "draws_matrix")
  colnames(pdata) <- colnames(res$standat$presence)
  kpost <- c(res$mod_res$draws(variables = "k", format = "draws_matrix")[, "k[1]"])



  nplots <- prod(mfrow)
  ids <- sample(colnames(pdata))
  if (!is.null(id)) {
    ids <- id
  }

  par(mfrow = mfrow)
  nchains <- max(cids)
  niter <- sum(cids == 1)
  cols <- hcl.colors(n = nchains)

  if (include_k) {
    plot(0, 0, ylim = range(kpost), xlim = c(0, niter), type = "n", xlab = "", ylab = "")
    for (i in seq_len(nchains)) {
      x <- seq_len(niter)
      y <- kpost[cids == i]
      points(x, y, type = "l", lwd = 0.5, col = cols[i])
    }
    legend("topright", legend = "k value", bg = "white", bty = "n", text.font = 2)
  }

  for (xi in seq_len(nplots - include_k)) {
    zz <- pdata[, ids[xi]]
    plot(0, 0, ylim = range(zz), xlim = c(0, niter), type = "n", xlab = "", ylab = "")
    for (i in seq_len(nchains)) {
      x <- seq_len(niter)
      y <- zz[cids == i]
      points(x, y, type = "l", lwd = 0.5, col = cols[i])
    }
    legend("topright", legend = ids[xi], bg = "white", bty = "n", text.font = 2)
  }

  par(opar)
}
