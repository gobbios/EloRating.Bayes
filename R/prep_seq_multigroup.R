#' prep interaction data for multi-group Stan model
#'
#' @param winner character
#' @param loser character
#' @param group character
#' @param dates integer
#' @param presence character
#' @param print_info logical, should info messages be returned (printed)
#'        (default is \code{TRUE}). Turn this off when running simulations.
#'
#' @return a list
#'
#' @examples
#' x <- group_mixture(n_ind = 20, n_mig = 4, n_grp = 3, n_int = 200)
#' s <- EloRating.Bayes:::prep_seq_multigroup(winner = x$x$winner,
#'                                            loser = x$x$loser,
#'                                            group = x$x$group,
#'                                            dates = x$x$dates,
#'                                            presence = x$p)
#' table(names(s$start_rating_index_indi))

# x <- group_mixture(n_ind = 12, n_mig = 2, n_grp = 2, n_abs = 2, n_int = 40)
# winner=x$x$winner
# loser=x$x$loser
# group=x$x$group
# dates=x$x$date
# presence=x$p
# # presence_wide=x$p_wide
# print_info = TRUE

prep_seq_multigroup <- function(winner, loser, group, dates, presence, print_info = TRUE) {
  if (is.factor(winner)) winner <- as.character(winner)
  if (is.factor(loser)) loser <- as.character(loser)
  if (is.factor(group)) group <- as.character(group)
  # group <- as.integer(as.factor(group))

  mig_table <- table(c(winner, loser), c(group, group)) > 0
  mig_ids <- NULL
  if (print_info) {
    aux <- rownames(mig_table)[rowSums(mig_table) > 1]
    if (length(aux) == 0) cat("no migrants detected\n")
    if (length(aux) > 0) cat("there were", length(aux), "migrants:", shQuote(aux), "\n")
    mig_ids <- aux
  }

  # make unique individuals (individual@group)
  winner <- paste0(winner, "@", group)
  loser <- paste0(loser, "@", group)

  all_ids <- unique(c(winner, loser))

  winner_index <- as.numeric(sapply(winner, function(x)which(all_ids == x)))
  loser_index <- as.numeric(sapply(loser, function(x)which(all_ids == x)))
  names(winner_index) <- winner
  names(loser_index) <- loser
  n_ind <- length(all_ids)
  n_int <- length(winner_index)

  grp_labels <- unique(group)
  n_grp <- length(unique(group))

  # translate presence matrix
  presence[is.na(presence)] <- "-99"

  p <- matrix(ncol = length(all_ids), nrow = n_int, 0)
  colnames(p) <- all_ids
  i=1
  for (i in seq_len(n_int)) {
    aux <- presence[i, presence[i, ] %in% group[i]]
    p[i, paste0(names(aux), "@", group[i])] <- 1
  }

  # sanity check:
  aux <- sapply(seq_len(n_int), function(x) {
    sum(presence[x, ] == group[x])
  })
  if (any(rowSums(p) != aux)) stop("error 1")

  # for inherit mode: get the index from which ratings are to be taken
  start_rating_index_indi <- rep(0, length = length(all_ids))
  names(start_rating_index_indi) <- colnames(p)
  i=1
  for (i in seq_len(nrow(mig_table))) {
    aux <- mig_table[i, ]
    id <- paste0(rownames(mig_table)[i], "@", colnames(mig_table)[aux])
    # non-migrants
    if (sum(aux) == 1) {
      start_rating_index_indi[id] <- 1
    }

    # migrants
    if (sum(aux) > 1) {
      aux2 <- apply(p[, id], 2, function(x)min(which(x == 1))) - 1
      aux2[which(aux2 == min(aux2))] <- 1
      start_rating_index_indi[id] <- aux2
    }
  }

  xgroup <- factor(group, levels = grp_labels)
  group_index <- as.integer(xgroup)
  names(group_index) <- group

  is_migrant <- start_rating_index_indi - start_rating_index_indi
  for (i in seq_along(mig_ids)) is_migrant[grepl(paste0(mig_ids[i], "@"), names(is_migrant))] <- 1

  standat <- list(n_ind = n_ind,
                  n_int = n_int,
                  n_grp = n_grp,
                  start_index = start_rating_index_indi,
                  is_migrant = is_migrant,
                  winner_index = winner_index,
                  loser_index = loser_index,
                  group_index = group_index,
                  presence = p,
                  group_size = rowSums(p)
  )




  # deal with migrants (those that occur in more than one group)
  # xtab <- table(c(group, group), c(winner, loser))
  # migs <- data.frame(interactions = as.numeric(xtab),
  #                    id = rep(colnames(xtab), each = n_grp),
  #                    group = rep(seq_len(n_grp), n_ind))
  # migs <- migs[migs$interactions > 0, ]
  # start_rating_index_indi <- as.numeric(sapply(migs$id, function(x)which(all_ids == x)))
  # names(start_rating_index_indi) <- all_ids[start_rating_index_indi]
  # start_rating_index_group <- migs$group
  # n_start <- length(start_rating_index_indi)


  # if (is.null(presence)) stop("requiring a properly formatted group-membership/presence matrix")
  # pmat <- matrix(ncol = n_start, nrow = n_int, 0)
  # start_index <- numeric(length = n_start)
  # presence[presence == 0] <- NA
  # colnames(pmat) <- names(start_rating_index_indi)
  # counter <- 1
  # i=unique(colnames(pmat))[2]
  # for (i in unique(colnames(pmat))) {
  #
  #   in_groups <- unique(na.omit(presence[, i]))
  #   if (length(in_groups) == 1) {
  #     pmat[, counter][!is.na(presence[, i])] <- in_groups
  #     counter <- counter + 1
  #     next
  #   }
  #
  #   for (k in in_groups) {
  #     pmat[, counter][which(presence[, i] == k)] <- k
  #     start_index[counter] <- which(presence[, i] == k)[1] - 1
  #     counter <- counter + 1
  #   }
  # }
  #
  # parray <- array(dim = c(n_grp, dim(pmat)), dimnames = list(NULL, NULL, colnames(pmat)))
  # gr=1
  # for (gr in seq_len(n_grp)) {
  #   ptemp <- pmat
  #   ptemp[ptemp != gr] <- 0
  #   ptemp[ptemp == gr] <- 1
  #   colnames(ptemp) <- colnames(pmat)
  #   parray[gr, , ] <- ptemp
  # }


  # standat <- list(n_ind = n_ind,
  #                 n_int = n_int,
  #                 n_grp = n_grp,
  #                 n_start = n_start,
  #                 start_rating_index_indi = start_rating_index_indi,
  #                 start_rating_index_group = start_rating_index_group,
  #                 winner_index = winner_index,
  #                 loser_index = loser_index,
  #                 group_index = group_index,
  #                 # presence = pmat,
  #                 # presence = parray,
  #                 start_index = start_index # take ratings at those 'dates' as start ratings
  #                 )


  standat
}
