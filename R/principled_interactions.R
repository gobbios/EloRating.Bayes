#' plotting ratings from interaction simulations
#'
#' @param rating_grid results from \code{\link{make_rating_grid}}
#' @param idata results from \code{\link{principled_interactions}}
#'
#' @returns a plot
#' @export
#'
#' @examples
#' rating_grid <- make_rating_grid(n_ind = 7, n_days = 100, drifts = 3)
#' plot_principled(rating_grid)
#'
#' idata <- principled_interactions(rating_grid = rating_grid, n_int = 27)
#' plot_principled(rating_grid, idata)
#'
#' rating_grid <- make_rating_grid(n_ind = 7, n_days = 100, start_sd = 0, drifts = 3)
#' plot_principled(rating_grid)

plot_principled <- function(rating_grid,
                            idata = NULL
                            ) {

  xgrid <- rating_grid$ratings_per_day
  colormat <- rating_grid$color_matrix
  plot(0, 0, "n",
       xlim = c(1, nrow(xgrid)),
       ylim = range(xgrid),
       xlab = "day", ylab = "rating")

  z <- sapply(seq_len(ncol(xgrid)), function(x) {
    points(xgrid[, x], type = "l")
    if (any(colormat[, x] == "red")) {
      points(which(colormat[, x] == "red"),
             xgrid[which(colormat[, x] == "red"), x],
             col = 'red', lwd = 3, type = "l")
    }
  })

  if (!is.null(idata)) {
    w <- idata$idata$winner
    l <- idata$idata$loser
    i <- idata$rating_grid
    d <- rownames(i)
    d <- sapply(idata$idata$date, \(x)which(x == d))

    w <- sapply(seq_len(length(w)), \(x) i[d[x], w[x]])
    l <- sapply(seq_len(length(l)), \(x) i[d[x], l[x]])

    points(d, w, pch = "|")
    points(d, l, pch = "|")
  }

  invisible(NULL)
}

#' generate dyadic dominance interactions
#'
#' @param rating_grid results from \code{\link{make_rating_grid}}
#' @param n_int numeric, number of interactions
#'
#' @returns a dataframe
#' @export
#'
#' @examples
#' xgrid <- make_rating_grid(n_ind = 7, n_days = 100, drifts = 1)
#' idata <- principled_interactions(rating_grid = xgrid, n_int = 27)
#' head(idata)

principled_interactions <- function(rating_grid,
                                    n_int = 26
                                    ) {
  # that's the function that gives the winprob for the first (r1)
  # it's the one in use in the Stan functions
  winprob <- function(r1, r2) {
    1/(1 + exp((r2 - r1)));
  }

  xgrid <- rating_grid$ratings_per_day
  n_ind <- ncol(xgrid)

  # dates (first and last date *have* to be included)
  xdates <- sample(rownames(xgrid), n_int, replace = TRUE)
  xdates[sample(n_int, 2)] <- range(rownames(xgrid))
  xdates <- sort(xdates)
  # pairs
  out <- t(sapply(seq_len(n_int), \(x)sample(seq_len(n_ind), 2, replace = FALSE)))
  # outcomes
  expected <- logical(n_int)
  winners_prob <- numeric(n_int)
  i=1
  for (i in seq_len(n_int)) {
    wp <- winprob(xgrid[xdates[i], out[i, 1]], xgrid[xdates[i], out[i, 2]])
    r <- rbinom(1, 1, wp)
    if (r == 0) out[i, ] <- rev(out[i, ])
    expected[i] <- xgrid[xdates[i], out[i, 1]] > xgrid[xdates[i], out[i, 2]]
    winners_prob[i] <- winprob(xgrid[xdates[i], out[i, 1]], xgrid[xdates[i], out[i, 2]])
  }

  out <- data.frame(winner = out[, 1], loser = out[, 2])
  out$date <- xdates
  out$expected <- expected
  out$winners_prob <- winners_prob

  if (n_ind <= 26) {
    out$winner <- letters[out$winner]
    out$loser <- letters[out$loser]
    colnames(xgrid) <- letters[1:n_ind]
  }

  out <- list(idata = out, rating_grid = xgrid)
  out
}


#' create rating matrix per day
#'
#' as basis for simulating interactions
#'
#' @param n_ind numeric, number of individuals (defaul is \code{10})
#' @param n_days numeric, number of days (defaul is \code{100})
#' @param start_sd positive numeric, SD of start ratings (defaul is \code{1})
#' @param drifts numeric, the number of 'rank change' events
#'          (default is \code{0}). When this is 0, the system is temporarilly
#'          stable.
#'
#' @returns a list
#' @export
#'
#' @details
#' if start_sd = 0: internally, this will be change to 0.000001, so that no
#'   winprob of exactly 50% can occur. Practically, this doesn't make a
#'   difference except for the expectation column.
#'
#' @examples
#' lapply(make_rating_grid(7, 100, drifts = 1), head, 3)

make_rating_grid <- function(n_ind = 10,
                             n_days = 100,
                             start_sd = 1,
                             drifts = 0) {

  if (start_sd == 0) start_sd <- 1e-06
  start_ratings <- as.numeric(scale(rnorm(n_ind))) * start_sd

  xgrid <- matrix(ncol = n_ind, nrow = n_days,
                  data = start_ratings, byrow = TRUE)
  d <- seq.Date(as.Date("2000-01-01"), length.out = nrow(xgrid), by = "day")
  rownames(xgrid) <- as.character(d)

  # map and track matrix for fixing ids
  mapmat <- xgrid > -Inf
  colormat <- matrix(data = "black", ncol = n_ind, nrow = n_days)
  drift_list <- list()
  # indicator for days with drift events
  drift_per_day <- rep(0, n_days)


  for (i in seq_len(drifts)) {
    stilllooking <- TRUE
    while (stilllooking) {
      t_delta <- 30
      k <- 2
      id <- sample(n_ind, 1)
      t0 <- sample(2:(n_days - (t_delta + 1)), 1)
      # ensuring that within individuals changes are progressive
      # i.e. block everything up to t0
      loc_vec <- t0:(t0 + t_delta)
      if (any(!mapmat[loc_vec, id])) next
      mapmat[1:(t0 + t_delta), id] <- FALSE

      from <- xgrid[t0, id]
      (xdelta <- runif(1, -2, 2))
      drift_res <- rating_drift(from = from, rat_delta = xdelta,
                                t0 = t0, t_delta = t_delta,
                                k = k, sel_id = id)
      newvals <- drift_res$drift_vec

      xgrid[t0:(t0 + t_delta), id] <- newvals
      colormat[t0:(t0 + t_delta), id] <- "red"
      xgrid[(t0 + t_delta):n_days, id] <- from + xdelta
      stilllooking <- FALSE

      # housekeeping
      # center
      xgrid <- xgrid - rowMeans(xgrid)
      drift_per_day[loc_vec] <- drift_per_day[loc_vec] + 1
    }
    drift_list[[length(drift_list) + 1]] <- drift_res$input
  }

  out <- list(ratings_per_day = xgrid,
              color_matrix = colormat,
              drift_list = drift_list,
              drift_per_day = drift_per_day)
  class(out) <- "rating_grid"
  out
}

#' vector with rating drift (sigmoidal)
#'
#' simulating 'rank changes'
#'
#' @param from numeric, rating from which the drift starts
#' @param rat_delta numeric, change magnitude (positive = rating increase,
#'          negative = rating decrease)
#' @param t0 integer, start day
#' @param t_delta integer (default is \code{30}), duration of the drift
#' @param k numeric, steepness of the slope (default is \code{2})
#' @param sel_id numeric, placeholder for individual (to be put in the output).
#'          Has no effect on any calculation.
#'
#' @returns a list with two items: a vector of length \code{t_delta + 1} with
#'   new ratings, and a vector with values of arguments that were used
#' @export
#'
#' @examples
#' rating_drift(from = 2, rat_delta = 0.8, t0 = 1, t_delta = 6, k = 2)
#' rating_drift(from = 2, rat_delta = 0.8, t0 = 1, t_delta = 6, k = 0.1)
#' rating_drift(from = 2, rat_delta = 0.8, t0 = 1, t_delta = 6, k = 10)
rating_drift <- function(from,
                         rat_delta,
                         t0,
                         t_delta = 30,
                         k = 2,
                         sel_id = 0
                         ) {
  to <- from + rat_delta
  t1 <- t0 + t_delta
  t <- t0:t1
  tmid <- (t0 + t1) / 2
  direction <- -sign(rat_delta)
  res <- from + (to - from) / (1 + exp(-k * direction * (t - tmid)))
  if (rat_delta > 0) res <- rev(res)

  list(drift_vec = res,
       input = c(from = as.numeric(from), rat_delta = rat_delta, t0 = t0,
                 t_delta = t_delta, k = k, sel_id = sel_id))
}

