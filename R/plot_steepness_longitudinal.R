#' plot steepness posteriors at multiple time steps throughout a sequence
#'
#' @param res result of \code{\link{elo_seq_bayes}}
#' @param dates character of target date(s) (YYYY-MM-DD)
#' @param vertical_stretch numeric, expansion of the posteriors along the
#'                         horizontal axis. Setting this parameter is just
#'                         for appearance and for now requires a bit of
#'                         trial and error. Its value will depend on how
#'                         many steepness posteriors are going to be displayed
#'                         and how long the date range is in the data.
#'                         The default is 200.
#'
#' @return a plot (and an invisible \code{NULL})
#' @export
#'
#' @examples
#' idata <- generate_interactions(n_ind = 8, n_int = c(300, 300), steep = c(0.9, 0.2))
#' res <- elo_seq_bayes(prep_seq(winner = idata$winner, loser = idata$loser, Date = idata$date), parallel_chains = 4)
#' dates <- c(idata$date[max(which(idata$set == 1))], idata$date[max(which(idata$set == 2))])
#' plot_steepness_longitudinal(res = res, dates = dates)

plot_steepness_longitudinal <- function(res, dates, vertical_stretch = 200) {
  post_steep <- longitudinal_steepness(res = res, dates = dates)

  drange <- range(as.numeric(as.Date(names(res$standat$idates))))
  firstday <- drange[1]
  drange <- drange - firstday
  plot(0, 0, type = "n", xlim = drange * c(1, 1.2), ylim = c(0, 1), yaxs = "i", las = 1, axes = FALSE, ylab = "steepness", xlab = "")
  axis(2, las = 1)
  i=1
  for (i in seq_len(ncol(post_steep))) {
    temp <- density(post_steep[, i])

    # x-axis: convert density to date (and stretch)
    aux <- temp$y / max(temp$y)
    xvals <- as.numeric(as.Date(as.character(dates)))[i] - firstday + aux * vertical_stretch
    # y-axis: the original x
    yvals <- temp$x
    points(x = xvals, y = yvals, type = "l", xpd = TRUE)
  }

  # axis(1, at = dates - firstday, labels = as.character(dates))
  text(x = as.numeric(as.Date(as.character(dates))) - firstday, y = -0.05, xpd = TRUE, adj = 1, srt = 45, labels = as.character(dates), cex = 0.5)
  invisible(NULL)
}
