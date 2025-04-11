#' extract ratings
#'
#' @param res result of \code{\link{elo_seq_bayes}}
#' @param targetdate vector of target dates (date format: YYYY-MM-DD). At its
#'                   default (\code{NULL}), the date of the last observed
#'                   interaction is used. If set to \code{0}, the start ratings
#'                   are returned.
#' @param make_summary logical, provide summary, or if \code{FALSE}: provide
#'                posterior draws
#' @param quiet logical, default is \code{TRUE}: try to capture all output that
#'              is normally printed to the console (i.e. informational messages
#'              from cmdstanr). If \code{FALSE}: print output to console
#'              (helpful for debugging and to monitor progress).
#' @param sel_draws numeric, if specified randomly select this number of
#'                  posterior draws (mostly useful for debugging and testing).
#'                  At its default (\code{NULL}) all draws are considered.
#'                  This argument is only meaningful when
#'                  \code{make_summary = FALSE}, i.e. when full posteriors
#'                  are requested.
#' @param keep_absent logical with default \code{TRUE}. See details section.
#' @param point_presence logical with default \code{TRUE}: use presence on
#'          the exact \code{targetdate}. If \code{FALSE}: consider animals
#'          present if they were present on \emph{any} day between the
#'          \code{targetdate} and the previous data (determined by
#'          \code{resol}). This only has effects if \code{targetdate}
#'          is a vector with more than one entry.
#'
#' @details
#' \code{keep_absent} is important for how individuals are treated that were
#'   not present (according to presence data) for a given date.
#'   This is only relevant if
#'   presence data was supplied to \code{\link{prep_seq}} in the first place.
#'   If \code{keep_absent = FALSE}, absent individuals are entirely removed
#'   from the output. If \code{keep_absent = TRUE}, these individuals will
#'   be present in the output, but all their values will be set to \code{NA}.
#'
#' @return if \code{make_summary=TRUE}: a data frame; otherwise a list of
#'         matrices in which each matrix represent individuals in columns and
#'         the individual draws in rows. Each list item then corresponds to
#'         the desired date
#' @export
#' @importFrom posterior as_draws_matrix
#' @importFrom cmdstanr draws_to_csv
#' @importFrom utils capture.output
#'
#' @examples
#' library(EloRating)
#' x <- randomsequence(nID = 8, avgIA = 50)$seqdat
#' # make date column heterogeneous (but still ordered)
#' x$Date <- sort(sample(x$Date, nrow(x), TRUE))
#' standat <- prep_seq(winner = x$winner, loser = x$loser, Date = x$Date)
#' res <- elo_seq_bayes(standat, parallel_chains = 2)
#' extract_elo_b(res, c("2000-01-17", "2000-01-27"))
#' extract_elo_b(res, c("2000-01-17", "2000-01-27"), make_summary = FALSE, sel_draws = 3)
#'
#' # working with presence
#' data(adv)
#' data(advpres)
#' sdat <- prep_seq(winner = adv$winner, loser = adv$loser, Date = adv$Date,
#'                  presence = advpres)
#' res <- elo_seq_bayes(standat = sdat, parallel_chains = 2)
#' # select two target dates with absent individuals
#' targetdate <- c("2010-01-22", "2010-02-02")
#' # 'c' is missing on first date, 'f' on second
#' extract_elo_b(res, targetdate)
#' # remove them from output
#' extract_elo_b(res, targetdate, keep_absent = FALSE)
#'
#' # with full posteriors
#' extract_elo_b(res, targetdate, make_summary = FALSE, sel_draws = 3)
#' extract_elo_b(res, targetdate, make_summary = FALSE, sel_draws = 3, keep_absent = FALSE)

extract_elo_b <- function(res,
                          targetdate = NULL,
                          make_summary = TRUE,
                          quiet = TRUE,
                          sel_draws = NULL,
                          keep_absent = TRUE,
                          point_presence = TRUE
                          ) {

  if (is.null(targetdate)) {
    targetdate <- max(as.Date(names(res$standat$idates)))
  }
  if (length(targetdate) == 1 && targetdate == 0) {
    targetdate <- min(as.Date(names(res$standat$idates)))
  }
  # initial date check (catch date errors first)
  tdates <- date2index(dateseq = as.Date(names(res$standat$idates)),
                       targetdate = as.Date(targetdate))

  # path for including standalone functions
  includepath <- normalizePath(file.path(system.file("extdata", package = "EloRating.Bayes"), "stan_functions"))

  if (quiet) {
    x <- capture.output(mod <- suppressMessages(cmdstan_model(system.file("extdata/elo_withdates.stan",
                                                         package = "EloRating.Bayes"),
                                             quiet = TRUE,
                                             include_path = includepath))
    )
  } else {
    mod <- cmdstan_model(system.file("extdata/elo_withdates.stan",
                                     package = "EloRating.Bayes"),
                         quiet = TRUE,
                         include_path = includepath)
  }
  res$standat$targetdates <- tdates
  res$standat$n_extract <- length(tdates)

  draws_csv_files <- draws_to_csv(as_draws_matrix(res$mod_res$draws()))

  if (quiet) {
    x <- capture.output(gq <- mod$generate_quantities(draws_csv_files, data = res$standat))
  } else {
    gq <- mod$generate_quantities(draws_csv_files, data = res$standat)
  }
  ids <- colnames(res$standat$presence)
  xdates <- as.character(targetdate)
  presence <- res$standat$presence

  if (make_summary) {
    out <- as.data.frame(gq$summary(variables = "out_perdate"))
    out <- data.frame(id = out$variable, date = NA, present = FALSE, out[, -1], check.names = FALSE)

    vars <- out$id
    vars <- gsub("out_perdate[", "", vars, fixed = TRUE)
    vars <- gsub("]", "", vars, fixed = TRUE)
    vars <- apply(do.call("rbind", strsplit(vars, ",")), 2, as.numeric)

    out$id <- ids[vars[, 2]]
    out$date <- xdates[vars[, 1]]

    # presence[tdates, , drop = FALSE]
    smallpmat <- presence[tdates, , drop = FALSE]
    if (!point_presence && length(tdates) >= 2) {
      for (i in 2:length(tdates)) {
        smallpmat[i, ] <- as.numeric(colSums(presence[tdates[i] : tdates[i - 1], ]) > 0)
      }

    }
    # if (any(colSums(smallpmat) == 0)) {
    #   warning("there are individuals in the data set that were never present on\n",
    #           "any of the targetdates.\n")
    # }
    out$present <- as.logical(smallpmat)


    if (!keep_absent) {
      out <- out[out$present, ]
      rownames(out) <- NULL
    } else {
      for (i in which(!out$present)) {
        out[i, which(!colnames(out) %in% c("id", "date", "present"))] <- NA
      }
    }

  }

  if (!make_summary) {
    draws <- as.matrix(gq$draws(variables = "out_perdate", format = "draws_matrix"))
    if (!is.null(sel_draws)) {
      draws <- draws[sample(nrow(draws), sel_draws), ]
    }

    ids <- colnames(res$standat$presence)
    xdates <- as.character(targetdate)

    vars <- colnames(draws)
    vars <- gsub("out_perdate[", "", vars, fixed = TRUE)
    vars <- gsub("]", "", vars, fixed = TRUE)
    vars <- apply(do.call("rbind", strsplit(vars, ",")), 2, as.numeric)

    # replace column names with id names
    colnames(draws) <- ids[vars[, 2]]

    out <- lapply(seq_along(tdates), function(x) {
      aux <- draws[, seq(x, ncol(draws), by = length(tdates))]
      if (length(targetdate) >= 2 && !point_presence && x >= 2) {
        # aux[,"KirikuSouth"]
        # smallpmat[i, ] <- as.numeric(colSums(presence[tdates[i] : tdates[i - 1], ]) > 0)
        aux[, which(as.numeric(colSums(presence[tdates[x] : tdates[x - 1], ]) > 0) == 0)] <- NA
      } else {
        aux[, which(presence[tdates[x], ] == 0)] <- NA
      }
      aux
    })

    out <- lapply(out, function(x) {
      aux <- matrix(x, ncol = length(ids))
      colnames(aux) <- ids
      aux
    })
    # lapply(out, head)
    if (!keep_absent) {
      i=1
      for (i in seq_along(tdates)) {
        toremove <- c()
        if (length(targetdate) >= 2 && !point_presence && i >= 2) {

        } else {
          toremove <- which(presence[tdates[i], ] == 0)
        }

        if (length(toremove) > 0) {
          out[[i]] <- out[[i]][, -c(toremove)]
        }
      }
    }

    names(out) <- as.character(xdates)
  }

  out
}

