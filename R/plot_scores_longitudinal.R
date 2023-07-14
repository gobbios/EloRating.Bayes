
#' plot ratings longitudinally (experimental!)
#'
#' @param res result of \code{\link{elo_seq_bayes}}
#' @param date_range character of date range (YYYY-MM-DD): must be of length = 2;
#'                   if \code{NULL}: the entire detected range of dates is used.
#' @param unc_width numeric between 0 and 1, width of uncertainty around median
#'                  (default is 0.89).
#' @param resol numeric, number of steps
#'
#' @return a plot
#' @export
#' @importFrom graphics axis.Date points segments text
#' @importFrom grDevices grey
#' @importFrom stats runif
#'
#' @examples
#' x <- randomsequence(nID = 7, avgIA = 50, presence = c(0.4, 0.6))
#' standat <- prep_seq(winner = x$seqdat$winner, loser = x$seqdat$loser,
#'                     Date = x$seqdat$Date, presence = x$pres)
#' res <- elo_seq_bayes(standat = standat, refresh = 0, parallel_chains = 4)
#' plot_scores_longitudinal(res, resol = 7)

plot_scores_longitudinal <- function(res, date_range = NULL, unc_width = 0.89, resol = 11) {
  if (is.null(date_range)) {
    date_range <- range(as.Date(names(res$standat$idates)))
  } else {
    date_range <- as.Date(date_range)
  }

  # check if resolution is too large
  idaterangemax <- as.numeric(diff(range(as.Date(names(res$standat$idates))))) + 1
  if (resol > idaterangemax) {
    stop(paste0("resolution set too high (can't be larger than ", idaterangemax, ")"), call. = FALSE)
  }

  targetdate <- seq.Date(date_range[1], date_range[2], length.out = resol)

  pdata <- extract_elo_b(res = res, targetdate = targetdate, make_summary = FALSE, quiet = TRUE, keep_absent = FALSE)
  # lapply(pdata, ncol)
  # range(unlist(lapply(pdata, range)))

  ids <- colnames(res$standat$presence)


  # prepare plotting data
  pd <- list()

  i=ids[1]
  for (i in ids) {
    aux <- lapply(pdata, function(x) {
      if (i %in% colnames(x)) {
        quantile(x[, i], probs = c((1 - unc_width)/2, 0.5, 1 - (1 - unc_width)/2))
      } else {
        rep(NA, 3)
      }

    })

    aux <- data.frame(do.call("rbind", aux))
    aux$date <- as.Date(rownames(aux))
    aux$id <- i

    aux$stint <- 0

    if (!is.na(aux[1, 3])) aux$stint[1] <- 1
    i=2
    for (i in 2:nrow(aux)) {
      if (!is.na(aux[i - 1, 3]) & !is.na(aux[i, 3])) {
        aux$stint[i] <- max(aux$stint)
      }
      if (is.na(aux[i - 1, 3]) & !is.na(aux[i, 3])) {
        aux$stint[i] <- max(aux$stint) + 1
      }

    }


    pd[[length(pd) + 1]] <- aux
  }

  # y-range
  yr <- range(unlist(lapply(pd, function(x)range(x[, 1:3], na.rm = TRUE))))

  plot(0, 0, xlim = date_range, ylim = yr, type = "n", axes = FALSE,
       xlab = "Date", ylab = "rating")
  axis(2, las = 1)
  axis.Date(1, targetdate, format = "%Y-%m")

  # polygons
  i=1
  for (i in seq_along(ids)) {
    p <- pd[[i]]
    k = 1
    for (k in 1:max(p$stint)) {
      aux <- p[p$stint == k, ]
      if (nrow(aux) > 1) {
        polygon(x = c(aux$date, rev(aux$date)), y = c(aux[, 1], rev(aux[, 3])), col = grey(0.8, 0.5), border = NA)
      }
      if (nrow(aux) == 1) {
        segments(aux$date, aux[, 1], aux$date, aux[, 3], col = "darkgrey")
      }
    }
  }

  # lines
  for (i in seq_along(ids)) {
    p <- pd[[i]]
    k = 1
    for (k in 1:max(p$stint)) {
      aux <- p[p$stint == k, ]
      if (nrow(aux) > 1) {
        points(aux$date, aux[, 2], type = "l")
      }
      if (nrow(aux) == 1) {
        points(aux$date, aux[, 2], pch = 21, col = "black", bg = "grey")
      }
    }
  }

  invisible(pd)
}
