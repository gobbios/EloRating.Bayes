#' simulate interactions with a group split
#'
#' group splits into two distinct subsets (groups) of individuals
#'
#' @param n_ind number of individuals
#' @param n_int number of interactions
#' @param split_by_matriline \code{"hard"} or \code{"random"}
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' x <- sim_group_split(n_ind = 12, n_int = 800, split_by_matriline = "hard")
#' standat <- prep_seq(winner = x$g1data$winner, loser = x$g1data$loser, Date = x$g1data$date)
#' r <- elo_seq_bayes(standat, quiet = TRUE, parallel_chains = 4, iter_sampling = 200, iter_warmup = 200)
#' plot_scores_longitudinal(r, plot_presence = x$p1, unc_width = 0.0)
#' }
#'




# n_ind = 5; n_int = 25
# split_by_matriline="hard"
sim_group_split <- function(n_ind = 15, n_int = 100, split_by_matriline = c("hard", "random")) {
  if (missing(split_by_matriline)) split_by_matriline <- "hard"
  n_matri <- 2
  if (n_matri * 2 > n_ind) stop("need at least 2 individuals per matriline")
  ids <- rep("a", n_ind)

  while (length(unique(ids)) != n_ind) {
    ids <- sapply(seq_len(n_ind), function(x)paste(sample(letters, 2), collapse = ""))
  }

  df <- data.frame(ids = ids)
  df$matri <- sample(x = n_matri, size = n_ind, replace = TRUE)
  df$matri[seq_len(n_matri * 2)] <- rep(seq_len(n_matri), each = 2)

  rownames(df) <- ids
  df <- df[order(df$matri), ]
  rats <- as.numeric(scale(rnorm(n_ind)))
  df$rat <- sort(rats)


  df$endsupin <- df$matri
  if (split_by_matriline == "hard") {
    df$endsupin <- df$matri
  }
  if (split_by_matriline == "random") {
    df$endsupin <- sample(df$endsupin)
  }


  idata <- data.frame(n = seq_len(n_int), split = TRUE, winner = NA, loser = NA)
  idata$date <- seq.Date(as.Date("1990-01-01"), length.out = n_int, by = "day")
  idata$split[1:n_int/2] <- FALSE
  idata$group <- 0
  idata$group[idata$split] <- sample(seq_len(n_matri), sum(idata$split), replace = TRUE)

  # generate interactions
  i=1
  for (i in 1:nrow(idata)) {
    if (idata$group[i] == 0) {
      indis <- sample(df$ids, 2)
    } else {
      # g <- sample(n_matri, 1)
      indis <- sample(df$ids[df$endsupin == idata$group[i]], 2)
    }

    df[indis, ]
    (aux <- plogis(diff(df[indis, "rat"])))
    sample(df[indis, "ids"], 1, prob = c(1 - aux, aux))
    idata$winner[i] <- sample(df[indis, "ids"], 1, prob = c(1 - aux, aux))
    idata$loser[i] <- df[indis, "ids"][df[indis, "ids"] != idata$winner[i]]

    if (idata$group[i] != 0) {
      if (df$endsupin[df$ids == idata$winner[i]] != df$endsupin[df$ids == idata$loser[i]]) stop()
    }

  }

  # remove 2 inidivuals for some amount of time...
  remids <- sample(df$ids, 2)
  foundone <- FALSE
  while (!foundone) {
    trydata <- idata
    enddate <- min(max(idata$date[idata$group %in% c(0, 1)]), max(idata$date[idata$group %in% c(0, 2)]))
    startdate <- max(min(idata$date[idata$group %in% c(0, 1)]), min(idata$date[idata$group %in% c(0, 2)]))
    if (enddate <= startdate) next
    sampledates <- seq.Date(startdate, enddate, by = "day")
    remdate1 <- sort(sample(sampledates, 2))
    remdate2 <- sort(sample(sampledates, 2))



    rem1 <- which((idata$winner == remids[1] | idata$loser == remids[1]) & idata$date >= remdate1[1] & idata$date <= remdate1[2])
    if (length(rem1) > 0) trydata <- trydata[-rem1, ]
    rem2 <- which((idata$winner == remids[2] | idata$loser == remids[2]) & idata$date >= remdate2[1] & idata$date <= remdate2[2])
    if (length(rem2) > 0) trydata <- trydata[-rem2, ]


    g1data <- trydata[trydata$group %in% c(0, 1), ]
    g2data <- trydata[trydata$group %in% c(0, 2), ]

    if (remdate1[1] < g1data$date[1]) next
    if (remdate1[1] < g2data$date[1]) next
    if (remdate2[1] < g1data$date[1]) next
    if (remdate2[1] < g2data$date[1]) next

    foundone <- TRUE
  }




  # presence for two groups
  p1 <- matrix(1, nrow = sum(idata$group %in% c(0, 1)), ncol = n_ind)
  colnames(p1) <- df$ids
  aux <- c(which(idata$split)[1]) # nrow(p1)
  selids <- df$ids[df$endsupin %in% 2]
  p1[aux:nrow(p1), selids] <- 0

  p2 <- matrix(1, nrow = sum(idata$group %in% c(0, 2)), ncol = n_ind)
  colnames(p2) <- df$ids
  aux <- c(which(idata$split)[1]) # nrow(p1)
  selids <- df$ids[df$endsupin %in% 1]
  p2[aux:nrow(p2), selids] <- 0

  sel <- unique(date2index(dateseq = idata$date[idata$group %in% c(0, 1)], targetdate = seq.Date(remdate1[1], remdate1[2], by = "day")))
  p1[sel, remids[1]] <- 0
  sel <- unique(date2index(dateseq = idata$date[idata$group %in% c(0, 1)], targetdate = seq.Date(remdate2[1], remdate2[2], by = "day")))
  p1[sel, remids[2]] <- 0

  sel <- unique(date2index(dateseq = idata$date[idata$group %in% c(0, 2)], targetdate = seq.Date(remdate1[1], remdate1[2], by = "day")))
  p2[sel, remids[1]] <- 0
  sel <- unique(date2index(dateseq = idata$date[idata$group %in% c(0, 2)], targetdate = seq.Date(remdate2[1], remdate2[2], by = "day")))
  p2[sel, remids[2]] <- 0


  if (interactive()) print(df[remids, ])
  list(g1data = g1data, p1 = p1, g2data = g2data, p2 = p2, indi_data = df)
}
