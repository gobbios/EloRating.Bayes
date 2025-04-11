
#' plot ratings longitudinally (experimental!)
#'
#' @param res result of \code{\link{elo_seq_bayes}}
#' @param date_range character of date range (YYYY-MM-DD): must be of length = 2;
#'                   if \code{NULL}: the entire detected range of dates is used.
#' @param unc_width numeric between 0 and 1, width of uncertainty around median
#'                  (default is 0.89).
#' @param resol numeric, number of steps
#' @param plot_presence optional presence matrix used for plotting (see \code{\link{sim_group_split}})
#'
#' @return a plot
#' @export
#' @importFrom graphics axis.Date points segments text
#' @importFrom grDevices grey
#' @importFrom stats runif
#'
#' @examples
#' x <- EloRating::randomsequence(nID = 7, avgIA = 50, presence = c(0.4, 0.6))
#' standat <- prep_seq(winner = x$seqdat$winner, loser = x$seqdat$loser,
#'                     Date = x$seqdat$Date, presence = x$pres)
#' res <- elo_seq_bayes(standat = standat, refresh = 0, parallel_chains = 4)
#' plot_scores_longitudinal(res, resol = 7)
#'
#'
#' x <- sim_group_split(n_ind = 11, n_int = 700, split_by_matriline = "hard")
#' standat <- prep_seq(winner = x$g1data$winner, loser = x$g1data$loser, Date = x$g1data$date)
#' r <- elo_seq_bayes(standat, quiet = TRUE, chains = 1, iter_sampling = 200, iter_warmup = 200)
#' plot_scores_longitudinal(r, plot_presence = x$p1, unc_width = 0.0, resol = 9)
#'




# res=r
# date_range = NULL
# unc_width = 0.89
# resol = 7
# plot_presence=NULL
plot_scores_longitudinal <- function(res,
                                     date_range = NULL,
                                     unc_width = 0.89,
                                     resol = 11,
                                     plot_presence = NULL
                                     ) {
  if (is.null(date_range)) {
    date_range <- range(as.Date(names(res$standat$idates)))
  } else {
    date_range <- as.Date(date_range)
  }

  # check if resolution is too large
  idaterangemax <- as.numeric(diff(range(as.Date(names(res$standat$idates))))) + 1
  if (resol > idaterangemax) {
    stop(paste0("resolution set too high (can't be larger than ",
                idaterangemax,
                ")"),
         call. = FALSE)
  }

  targetdate <- seq.Date(date_range[1], date_range[2], length.out = resol)
  o <- find_origin(targetdate[1])
  targetdate <- as.Date(as.integer(targetdate), origin = o) # needs to be integer otherwise R on Windows makes trouble

  pdata <- extract_elo_b(res = res, targetdate = targetdate, make_summary = FALSE,
                         quiet = TRUE, keep_absent = FALSE, point_presence = FALSE)
  # lapply(pdata, ncol)
  # lapply(pdata, head)
  # range(unlist(lapply(pdata, range)))


  if (!is.null(plot_presence)) {
    di <- date2index(dateseq = as.Date(names(res$standat$idates)), targetdate = targetdate)
    xplotpresence <- plot_presence[di, ]
  }


  ids <- colnames(res$standat$presence)


  # prepare plotting data
  pd <- list()

  i=ids[20]
  for (i in ids) {
    aux <- lapply(pdata, function(x) {
      if (i %in% colnames(x)) {
        if (all(is.na(x[, i]))) return(rep(NA, 3))
        quantile(x[, i], probs = c((1 - unc_width)/2, 0.5, 1 - (1 - unc_width)/2))
      } else {
        rep(NA, 3)
      }

    })

    aux <- data.frame(do.call("rbind", aux))
    aux$date <- as.Date(rownames(aux))
    aux$id <- i

    aux$stint <- 0
    if (!is.null(plot_presence)) {
      aux[xplotpresence[, i] == 0, 3] <- NA
    }
    if (!is.na(aux[1, 3])) aux$stint[1] <- 1
    auxrow=2
    for (auxrow in 2:nrow(aux)) {
      if (!is.na(aux[auxrow - 1, 3]) & !is.na(aux[auxrow, 3])) {
        aux$stint[auxrow] <- max(aux$stint)
      }
      if (is.na(aux[auxrow - 1, 3]) & !is.na(aux[auxrow, 3])) {
        aux$stint[auxrow] <- max(aux$stint) + 1
      }

    }

    if (!is.null(plot_presence)) {
      # aux[xplotpresence[, i] == 0, 1] <- NA
      # aux[xplotpresence[, i] == 0, 2] <- NA
      # aux[xplotpresence[, i] == 0, 3] <- NA
      # aux <- aux[xplotpresence[, i] == 1, , drop = FALSE]
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
