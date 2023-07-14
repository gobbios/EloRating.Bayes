#' cross-sectional plot of rating posteriors
#'
#' @param res result of \code{\link{elo_seq_bayes}}
#' @param targetdate character of target date (YYYY-MM-DD):
#'                   must be of length = 1
#' @param color logical, default is \code{TRUE} where individuals
#'        get color-coded. If \code{FALSE}: a gray scale is used.
#'        It is also possible to hand over a vector with colors,
#'        which then must correspond in length to the number of individuals.
#'
#' @return a plot
#' @export
#' @importFrom stats density
#' @importFrom graphics axis polygon title
#' @importFrom grDevices gray.colors hcl.colors
#' @examples
#' library(EloRating)
#' x <- randomsequence(nID = 7, avgIA = 100)$seqdat
#' standat <- prep_seq(winner = x$winner, loser = x$loser, Date = x$Date)
#' res <- elo_seq_bayes(standat = standat, refresh = 0, parallel_chains = 2)
#' plot_scores(res, targetdate = "2000-01-01")
#' title(main = "start ratings")
#' plot_scores(res, targetdate = rev(names(standat$idates))[1])
#' title(main = "ratings at sequence end")

plot_scores <- function(res,
                        targetdate = NULL,
                        color = TRUE) {

  if (is.null(targetdate)) {
    targetdate <- max(as.Date(names(res$standat$idates)))
  }

  if (length(targetdate) > 1) stop("can only handle one date")

  pdata <- extract_elo_b(res = res, targetdate = targetdate, make_summary = FALSE, quiet = TRUE, keep_absent = FALSE)
  pdata <- pdata[[1]]
  n_ids <- ncol(pdata)
  text_locs <- colMeans(pdata)
  text_labs <- colnames(pdata)

  pdata <- apply(pdata, 2, density, adjust = 1) # adjustpar
  pmax <- max(unlist(lapply(pdata, function(x) max(x$y))))

  xl <- range(unlist(lapply(pdata, function(x) range(x$x))))
  yl <- c(0, pmax * 1.05)

  if (!isFALSE(color) & !isTRUE(color) & !is.null(color)) {
    cols <- NULL
    if (length(color) == n_ids) {
      cols <- color
    }
    if (length(color) == 1) {
      cols <- rep(color, n_ids)
    }
    if (is.null(cols)) {
      stop("colour vector does not match number of ids")
    }
  }
  if (isTRUE(color)) {
    cols <- sample(hcl.colors(n = n_ids, "zissou", alpha = 0.7))
  }
  if (isFALSE(color)) {
    cols <- sample(gray.colors(n = n_ids, start = 0.3, end = 0.9,
                               alpha = 0.7))
  }
  border_cols <- rep("black", n_ids)

  plot(0, 0, type = "n", xlim = xl, ylim = yl, yaxs = "i", xaxs = "i", axes = FALSE, xlab = "", ylab = "")
  title(ylab = "density", line = 1)
  title(xlab = "Elo-rating", line = 1.8)
  axis(1, gap.axis = 0.2, mgp = c(2, 0.7, 0), tcl = -0.3)
  for (i in seq_len(n_ids)) {
    p <- pdata[[i]]
    p$x[p$x > (n_ids - 1)] <- n_ids - 1
    polygon(c(p$x, rev(p$x)), c(rep(0, length(p$x)), rev(p$y)),
            border = NA, col = cols[i])
  }
  for (i in seq_len(n_ids)) {
    p <- pdata[[i]]
    p$x[p$x > (n_ids - 1)] <- n_ids - 1
    polygon(c(p$x, rev(p$x)), c(rep(0, length(p$x)), rev(p$y)),
            border = border_cols[i], col = NA, lwd = 0.4, xpd = TRUE)
  }

  # text labels
  yvals <- runif(n_ids, yl[2] * 0.98, yl[2] * 1.02)
  text(text_locs, yvals, text_labs, col = cols, xpd = TRUE)
}

