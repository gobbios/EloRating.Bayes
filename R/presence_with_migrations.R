#' generate presence matrix with migrations
#'
#' individuals can migrate multiple times (and also return to a group)
#'
#' @param n_ind integer, number of individuals
#' @param n_mig integer, number of migrations
#' @param n_grp integer, number of groups
#' @param n_days integer, number of days that the study covers
#' @param n_abs integer, number of absence-stints (see details)
#'
#' @details
#' \code{n_abs} determines the number of stints that are affected by absence.
#' An individual that migrates once has two stints. An individual that
#' never migrates has one stint. Any such stint can be selected for an
#' individual to be absent.
#'
#'
#'
#' @return a list with a presence matrix (one line per day,
#'         one column per individual), and a condensed data.frame with
#'         the same information
#' @export
#'

presence_with_migrations <- function(n_ind = 10, n_mig = 2, n_grp = 2, n_days = 100, n_abs = 2) {

  # condition to be met:
  # each group contains at least one individual at all times


  # if only one group: there can't be migration... (but absence)
  if (n_grp == 1) {
    n_mig <- 0
    if (interactive()) message("only one group: no migration possible (n_mig set to 0)")
  }


  # impossible/problematic input
  if (n_abs > n_ind + n_mig) stop("too many absence stints")
  if (n_grp > n_ind/2) stop("too many groups (", n_grp, ") for ", n_ind, " individuals")


  still_looking <- TRUE
  dates <- seq_len(n_days)

  while (still_looking) {
    # set.seed(1)

    gm <- data.frame(id = sort(c(seq_len(n_ind), sample(n_ind, n_mig, replace = TRUE))),
                     from = NA,
                     to = NA,
                     group = NA,
                     stint = NA,
                     migrant = NA)
    any(table(gm$id) == 2) && max(table(gm$id)) == 2

    i=1
    for (i in seq_len(n_ind)) {
      # resident only in one group:
      if (sum(gm$id == i) == 1) {
        xline <- which(gm$id == i)
        gm$from[xline] <- 1
        gm$to[xline] <- n_days
        gm$stint[xline] <- 1
        gm$group[xline] <- sample(seq_len(n_grp), 1)
        gm$migrant[xline] <- FALSE
      }

      # migrants:
      if (sum(gm$id == i) > 1) {
        n_stints <- sum(gm$id == i)
        xlines <- which(gm$id == i)
        gm$migrant[xlines] <- TRUE

        # begin dates
        bdates <- sort(c(1, sample(2:(n_days - 10), n_stints - 1)))
        # end dates
        edates <- c(bdates[2:(length(bdates))] - 1, n_days)
        for (k in seq_len(n_stints)) {
          gm$from[xlines[k]] <- bdates[k]
          gm$to[xlines[k]] <- edates[k]
          if (k == 1) gm$group[xlines[k]] <- sample(seq_len(n_grp), 1)
          if (k > 1) {
            # available groups for next stint:
            available <- seq_len(n_grp)[-gm$group[xlines[k - 1]]]
            if (length(available) == 1) gm$group[xlines[k]] <- available
            if (length(available) > 1) gm$group[xlines[k]] <- sample(available, 1)
          }
          gm$stint[xlines[k]] <- k
        }
      }
    }

    ## sanity check ----
    # residences need to be at least 5 days
    if (any((gm$to - gm$from) < 5)) {
      next
    }


    ## labels ----
    gm$label <- paste(gm$id, gm$group, gm$stint, sep = "_")

    # bring the whole thing into wide format ----
    # out <- matrix(ncol = n_ind + n_mig, nrow = n_days, "")
    # colnames(out) <- gm$label
    out <- matrix(ncol = n_ind, nrow = n_days, -1)
    colnames(out) <- seq_len(n_ind)

    for (i in seq_len(n_ind)) {
      temp <- gm[gm$id == i, ]
      for (k in seq_len(nrow(temp))) {
        out[temp$from[k]:temp$to[k], i] <- temp$group[k]
      }
    }

    # throw absence on top ----
    gm$absence <- FALSE
    gm$absence[sample(nrow(gm), n_abs)] <- TRUE
    gm$abs_from <- NA
    gm$abs_to <- NA
    i=1
    for (i in seq_len(sum(gm$absence))) {
      xline <- which(gm$absence)[i]
      xid <- gm$id[xline]
      xfrom <- gm$from[xline]
      xto <- gm$to[xline]
      xdays <- xfrom:xto
      if (runif(1) > 0.5) xdays <- rev(xdays)
      absent_days <- xdays[1:sample(2:floor(length(xdays) / 2), 1)]
      out[absent_days, xid] <- 0
      gm$abs_from[xline] <- min(absent_days)
      gm$abs_to[xline] <- max(absent_days)
    }

    # sanity check: ----
    # all days have at least one id of each group
    outcheck <- out
    outcheck[outcheck == 0] <- NA
    if(!all(apply(outcheck, 1, function(x) {
      all(table(x) >= 1) && length(table(x)) == n_grp
    }))) {
      next
    }


    # if we arrived here:
    still_looking <- FALSE

  }

  list(out = out, gm = gm)

}

