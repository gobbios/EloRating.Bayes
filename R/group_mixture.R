#' generate interaction data with migrating individuals
#'
#' each individual can migrate multiple times (and also back and forth)
#'
#' @param n_ind integer, number of individuals
#' @param n_mig integer, number of migrating individuals (must be
#'        smaller than \code{n_int})
#' @param n_grp integer, number of groups
#' @param n_int integer, number of interactions
#' @param n_abs integer, number of individuals that were at some point
#'              absent from the population (max absence time
#'              is half the study duration)
#' @param mig_mode determining the mode of how the migrators integrate
#'                 in their new group
#' @param silent logical, omit info messages or not
#' @param stop_at_iter numeric, number of iterations until function fails
#' @param fail_at_stop logical, if function is unsuccessful: return error or \code{NULL}
#'
#' @importFrom stats coef lm na.omit rbinom rnorm
#' @importFrom utils combn
#'
#' @return a list, where the first item is a data.frame and the second one is
#'         a presence/group allocation matrix
#' @export
#'
#' @details
#' The 'inherit' mode for migration is a bit misleading at the moment.
#' In fact, the start ratings are inherited, i.e. if an individual rises from
#' a low start rating to a high rating within its group and then migrates,
#' it 'inherits' the low rating.
#' Within the simulated data sets this is okay, because ratings are stable
#' within groups, but in natural settings this might not be realistic...
#'
#'
#' @examples
#' group_mixture()
#
# n_ind = 15
# n_mig = 3
# n_grp = 3
# n_int = 100
# n_abs = 3
# mig_mode="bottom"
# stop_at_iter=100
# fail_at_stop = TRUE
group_mixture <- function(n_ind = 10, n_mig = 2, n_grp = 2, n_abs = 2, n_int = 32,
                          mig_mode = c("average", "inherit", "bottom", "top"),
                          stop_at_iter = 500, silent = FALSE, fail_at_stop = TRUE) {

  bottom_val <- -1.5
  top_val <- 1.5

  group_ids <- seq_len(n_grp)
  names(group_ids) <- sample(LETTERS, n_grp)

  stopifnot(n_mig <= n_ind,
            n_grp > 1)
  stop_at_iter_counter <- 0

  # default to average as mig_mode (if unspecified)
  if (length(mig_mode) == 4) mig_mode <- "average"

  # iterate until interaction data contain interactions for all migrants in all groups
  still_looking <- TRUE

  while (still_looking) {
    stop_at_iter_counter <- stop_at_iter_counter + 1
    if (stop_at_iter_counter == stop_at_iter) {
      if (fail_at_stop) stop("stopped after ", stop_at_iter_counter, " iterations\n")
      return(NULL)
    }

    ids <- sort(sample(letters, n_ind))

    # start ratings for initial cohort
    indi_vec <- round(rnorm(n_ind, 0, 1), 2)
    ini_groups <- sample(seq_len(n_grp), n_ind, replace = TRUE)
    names(indi_vec) <- paste(ids, ini_groups, sep = "_")

    # matrix to assign the group to each individual
    group_alloc <- matrix(nrow = n_int, ncol = n_ind, data = ini_groups, byrow = TRUE)
    colnames(group_alloc) <- ids

    # settle migrations
    # currently it's still possible that migration occurs on last date
    # might want to leave some buffer at beginning and end?
    migrants <- sort(sample(ids, n_mig))
    i=migrants[1]
    for (i in migrants) {
      d <- sample(n_int - 1, 1) + 1
      old_group <- group_alloc[d - 1, i]
      if (n_grp == 2) new_group <- ifelse(old_group == 1, 2, 1)
      if (n_grp > 2) new_group <- sample(seq_len(n_grp)[-c(old_group)], 1)
      group_alloc[d:nrow(group_alloc), i] <- new_group
    }

    # throw in some absences (migration (to/from unknown groups), maturation, etc)
    # this might mask migration between groups (skip data set if this occurs)
    abs_inds <- sample(ids, n_abs)
    abs_length <- floor(n_int/2)
    i=1
    for (i in seq_len(n_abs)) {
      aux <- cbind((-abs_length + 1):(n_int + abs_length - 1), -1)
      s <- sample((-abs_length + 1):(n_int - 1), 1)
      aux[aux[, 1] %in% s:(s + abs_length), 2] <- 0
      aux <- which(aux[aux[, 1] %in% seq_len(n_int), 2] == 0)
      if (length(aux) > 0) {
        group_alloc[aux, abs_inds[i]] <- 0
      }
    }

    # make wide group allocation matrix (migrators get 1+ column: one column for each group they were in)
    gmat <- matrix(nrow = n_int, ncol = 0)

    # and another matrix reflecting the ratings depending on mig_mode
    rmat <- matrix(nrow = n_int, ncol = 0)


    i=ids[15]
    for (i in ids) {
      if (!i %in% migrants) {
        gn <- names(indi_vec)[grepl(paste0("^", i, "_"), names(indi_vec))]
        gmat <- cbind(gmat, group_alloc[, i])
        colnames(gmat)[ncol(gmat)] <- gn
        rmat <- cbind(rmat, indi_vec[gn])
        colnames(rmat)[ncol(rmat)] <- gn
        rmat[gmat[, gn] == 0, gn] <- NA
      } else {
        # stop()
        # gn <- names(indi_vec)[grepl(paste0("^", i, "_"), names(indi_vec))]
        g <- unique(group_alloc[group_alloc[, i] != 0, i])
        if (length(g) == 1) next # migrator not present in both groups
        # initial group
        aux <- group_alloc[, i]
        aux[aux != g[1]] <- 0
        gmat <- cbind(gmat, aux)
        colnames(gmat)[ncol(gmat)] <- paste(i, g[1], sep = "_")
        aux2 <- rep(indi_vec[paste(i, g[1], sep = "_")], n_int)
        aux2[gmat[, paste(i, g[1], sep = "_")] != g[1]] <- NA
        rmat <- cbind(rmat, aux2)
        colnames(rmat)[ncol(rmat)] <- paste(i, g[1], sep = "_")

        # after migration
        aux <- group_alloc[, i]
        aux[aux != g[2]] <- 0
        gmat <- cbind(gmat, aux)
        colnames(gmat)[ncol(gmat)] <- paste(i, g[2], sep = "_")


        aux <- group_alloc[, i]
        aux[aux != g[2]] <- NA
        if (mig_mode == "average") {
          new_rat <- 0
        }
        if (mig_mode == "inherit") {
          new_rat <- indi_vec[grepl(paste0("^", i, "_"), names(indi_vec))]
        }
        if (mig_mode == "bottom") {
          new_rat <- bottom_val
        }
        if (mig_mode == "top") {
          new_rat <- top_val
        }
        aux[!is.na(aux)] <- new_rat
        rmat <- cbind(rmat, aux)
        colnames(rmat)[ncol(rmat)] <- paste(i, g[2], sep = "_")
      }
    }
    # binarize
    gmat[gmat > 0] <- 1
    rownames(rmat) <- NULL

    # center within group
    for (i in 1:nrow(rmat)) {
      for (g in seq_len(n_grp)) {
        aux <- which(grepl(paste0("_", g), colnames(rmat)))
        rmat[i, aux] <- round(rmat[i, aux] - mean(rmat[i, aux], na.rm = TRUE), 2)
        # rmat[i, aux] <- round(as.numeric(scale(rmat[i, aux])), 2)
      }
    }
    # apply(rmat, 1, sd, na.rm=T)

    # sanity checks
    if (!all(colnames(gmat) == colnames(rmat))) stop("naming went wrong")
    table(unlist(lapply(strsplit(colnames(gmat), "_"), function(x)x[1])))


    # generate the interactions
    winner <- character(n_int)
    loser <- character(n_int)
    group <- character(n_int)
    dates <- integer(n_int)

    i=1
    for (i in seq_len(n_int)) {
      g <- sample(seq_len(n_grp), 1)
      pot_ids <- gmat[i, grepl(paste0("_", g), colnames(gmat)), drop = FALSE]
      pot_ids <- colnames(pot_ids)[pot_ids[1, ] > 0]
      if (length(pot_ids) < 2) next
      # sample dyad and determine winner
      dyad <- sample(pot_ids, 2)
      rats <- rmat[i, dyad]
      wp <- 1 / (1 + exp(rats[2] - rats[1]))
      (toss <- rbinom(1, 1, wp))
      if (toss == 0) {
        winner[i] <- dyad[2]
        loser[i] <- dyad[1]
      }
      if (toss == 1) {
        winner[i] <- dyad[1]
        loser[i] <- dyad[2]
      }

      group[i] <- g
      dates[i] <- max(dates) + sample(0:2, 1)
    }
    dates <- dates - min(dates) + 1

    # sanity check: interactions occur WITHIN groups
    aux1 <- unlist(lapply(strsplit(winner, "_"), function(x)x[2]))
    aux2 <- unlist(lapply(strsplit(loser, "_"), function(x)x[2]))
    if (any(is.na(aux1)) || any(is.na(aux2))) next
    if (!isTRUE(all(aux1 == aux2))) stop("error 2")

    # condition check: each id interacted at least two times in each of their groups
    if (any(table(c(winner, loser)) < 2)) next

    winner <- unlist(lapply(strsplit(winner, "_"), function(x)x[1]))
    loser <- unlist(lapply(strsplit(loser, "_"), function(x)x[1]))

    # check whether conditions are met
    ## all ids need to have interacted at least once
    if (!all(ids %in% unique(c(winner, loser)))) next

    ## absence doesn't start within a single day
    aux <- group_alloc[, abs_inds, drop = FALSE]
    aux <- apply(aux, 2, function(x)any(duplicated(unique(cbind(dates, x))[, 1])))
    if (any(aux)) next

    ## migrants still occur in two groups (which may be masked by absence)
    aux <- group_alloc[, migrants, drop = FALSE]
    aux <- apply(aux, 2, function(x)length(table(x[x != 0])) != 2)
    if (any(aux)) next

    testfine <- logical(2)

    ## migrants interacted at least once in each of their groups
    xtab <- colSums(table(c(group, group), c(winner, loser)) > 0)
    if (all(xtab[migrants] == 2)) testfine[1] <- TRUE
    ## migrants don't have interactions in two groups on the same date
    xtab <- cbind(c(group, group), c(winner, loser), c(dates, dates))
    xtab <- xtab[xtab[, 2] %in% migrants, ]
    aux <- tapply(xtab[, 1], list(xtab[, 2], xtab[, 3]), function(x)length(unique(x)) == 1)
    if (all(na.omit(as.logical(aux)))) testfine[2] <- TRUE

    if (all(testfine)) still_looking <- FALSE

  }

  # get true ratings vector
  true_ratings <- rmat[1, ]
  true_ratings[names(true_ratings) %in% names(indi_vec)] <- indi_vec
  true_ratings
  if (mig_mode == "inherit") {
    aux <- which(is.na(true_ratings))
    for (k in aux) {
      true_ratings[k] <- true_ratings[k - 1]
    }
  }
  if (mig_mode == "average") true_ratings[is.na(true_ratings)] <- 0
  if (mig_mode == "top") true_ratings[is.na(true_ratings)] <- top_val
  if (mig_mode == "bottom") true_ratings[is.na(true_ratings)] <- bottom_val
  true_ratings
  i=1
  for (i in seq_len(n_grp)) {
    names(true_ratings) <- gsub(paste0("_", i), paste0("@", names(group_ids)[group_ids[i]]), names(true_ratings))
    colnames(gmat) <- gsub(paste0("_", i), paste0("_", names(group_ids)[group_ids[i]]), colnames(gmat))
    group_alloc[group_alloc == i] <- names(group_ids)[group_ids[i]]
  }

  # additional matrix of migrants with initial and post-migration ratings
  # (for easier evaluation later on)
  eval_data <- expand.grid(id = migrants, id_grp = "", postmig = c(0, 1), rating = 0, stringsAsFactors = FALSE)
  i=migrants[1]
  for (i in migrants) {
    aux <- group_alloc[, i]
    aux <- unique(aux[aux != "0"])
    aux <- paste0(i, "@", aux)
    eval_data$rating[eval_data$id == i] <- true_ratings[aux]
    eval_data$id_grp[eval_data$id == i] <- aux
  }

  if (!silent) {
    if (length(intersect(migrants, abs_inds)) > 0) {
      cat("FYI: overlap between migrants and temporarily absent individuals\n")
    }
    cat("it took", stop_at_iter_counter, "iterations\n")
  }

  xdata <- data.frame(date = dates,
                      # winner = paste(winner, names(group_ids)[as.numeric(group)], sep = "_"),
                      # loser = paste(loser, names(group_ids)[as.numeric(group)], sep = "_"),
                      winner = winner,
                      loser = loser,
                      group = names(group_ids)[as.numeric(group)])
  list(x = xdata,
       p = group_alloc,
       # p_wide = gmat,
       true_ratings = true_ratings,
       eval_data = eval_data)
}
