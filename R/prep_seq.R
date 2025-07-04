#' prepare interaction data
#'
#' then to be used with \code{\link{elo_seq_bayes}}
#'
#' @param winner,loser character vector with ids of winner and loser individuals
#' @param Date Date or character vector with dates (\code{"YYYY-MM-DD"})
#' @param presence presence (one line per interaction, which is different from
#'                 the approach in the classic EloRating package!!!)
#' @param draws optional logical or integer (0/1) vector with information
#'              about draws (undecided/ties)
#' @param intensity optional character or factor describing type or intensity
#'          for each interaction. Determines how many k values are estimated.
#'          At its default (\code{NULL}), all interactions are considered of
#'          the same type/intensity and one k value is estimated.
#' @param is_male optional vector of 0 and 1 that encodes whether a individual
#'        is male. If provided this vector needs to be named and all
#'        individuals in \code{winner} and \code{loser} need to be included.
#'        Default is \code{NULL}, i.e. it is ignored.
#' @param estimate_startspread logical, default is \code{FALSE}. Should the SD
#'          of the start ratings be estimated. At its default, the SD is set to
#'          1. When the SD is estimated, its prior is \code{exponential(1);}.
#' @param extract_dates character or Date vector with dates. These ratings on
#'        these dates are generated in the Stan model and therefore easier
#'        to access during post processing. They do result in larger objects,
#'        though. The default is \code{NULL}, i.e., store ratings after the
#'        first interaction (i.e., not the actual start ratings) and
#'        ratings after the last interaction ("end of the sequence").
#' @param home_team,away_team,home_win for sports model
#'
#' @importFrom EloRating seqcheck
#' @return a list
#' @details see \code{\link{elo_seq_bayes}} for examples
#' @aliases prep_seq_sport


# x <- randomsequence(nID = 10, avgIA = 50, presence = c(0.2, 0.4))
# winner <- x$seqdat$winner
# loser <- x$seqdat$loser
# Date <- x$seqdat$Date
# presence <- x$pres


#' @export
prep_seq <- function(winner,
                     loser,
                     Date,
                     presence = NULL,
                     draws = NULL,
                     intensity = NULL,
                     estimate_startspread = FALSE,
                     extract_dates = NULL,
                     is_male = NULL
                     ) {

  if (is.factor(winner)) winner <- as.character(winner)
  if (is.factor(loser)) loser <- as.character(loser)
  if (is.numeric(winner) | is.numeric(loser)) {
    winner <- as.character(winner)
    loser <- as.character(loser)
    warning("winner and/or loser vectors were detected as being numeric. ",
            "This hasn't been tested, but should (should!) work. ",
            "Consider providing these data as character or factor.")
  }

  # generate warning if dates are not ordered
  if (any(order(unique(Date)) != seq_along(unique(Date)))) {
    warning("the interactions appear not to be ordered according to the ",
            "information in 'Date'\n",
            "please check that the order of interactions is correct",
            call. = FALSE)
  }

  if (is.null(draws)) {
    draws <- rep(0, length(winner))
  } else {
    draws <- as.integer(draws)
  }

  if (is.null(intensity)) {
    intensity <- rep("default", length(winner))
  }
  if (length(intensity) != length(winner)) {
    stop("intensity needs to be the same length as winner",call. =  FALSE)
  }

  aux <- unique(as.character(intensity))
  intensity_index <- as.numeric(sapply(intensity, function(x)which(aux == x)))
  names(intensity_index) <- intensity
  n_k <- length(aux)

  x <- seqcheck(winner, loser, Date)

  all_ids <- unique(c(winner, loser))
  winner_index <- as.numeric(sapply(winner, function(x)which(all_ids == x)))
  loser_index <- as.numeric(sapply(loser, function(x)which(all_ids == x)))
  names(winner_index) <- winner
  names(loser_index) <- loser
  n_ind <- length(all_ids)
  n_int <- length(winner_index)

  # handle sex difference
  if (!is.null(is_male)) {
    if (is.null(names(is_male))) stop("'is_male' requires names attribute")
    if (!all(names(is_male) %in% all_ids)) stop("individuals missing from 'is_male")
    is_male <- is_male[all_ids]
  } else {
    is_male <- numeric(n_ind)
    names(is_male) <- all_ids
  }

  if (is.null(presence)) {
    presence <- matrix(ncol = n_ind, nrow = n_int, 1)
    colnames(presence) <- all_ids
  } else {
    presence <- as.matrix(presence[, all_ids])
  }

  # make dates to be included in standata
  # workaround using a named vector of zeros
  xdates <- rep(0, n_int)
  names(xdates) <- as.character(as.Date(Date))

  standat <- list(n_ind = n_ind,
                  n_int = n_int,
                  n_k = n_k,
                  winner_index = winner_index,
                  loser_index = loser_index,
                  draws = draws,
                  intensity_index = intensity_index,
                  presence = presence,
                  is_male = is_male,
                  idates = xdates,
                  targetdates = find_extract_index(idates = Date,
                                                   edates = extract_dates),
                  n_extract = 0,
                  # default_extract_dates = as.integer(!is.null(extract_dates)),
                  startspread_fixed = 1,
                  startspread_val = 1
                  )
  standat$n_extract <- length(standat$targetdates)
  if (is.null(names(standat$targetdates))) {
    names(standat$targetdates) <- as.character(extract_dates)
  }

  if (estimate_startspread) {
    standat$startspread_val <- numeric(0)
    standat$startspread_fixed <- 0
  }


  standat
}

#' @rdname prep_seq
#' @export
prep_seq_sport <- function(winner, loser, Date, presence = NULL, draws = NULL,
                           home_team = NULL, away_team = NULL, home_win = NULL) {

  standat <- prep_seq(winner = winner, loser = loser, Date = Date, presence = presence, draws = draws)

  standat$homewin <- home_win
  standat$hometeam_index <- as.numeric(sapply(home_team, function(x)which(unique(c(names(standat$winner), names(standat$loser))) == x)))
  standat$awayteam_index <- as.numeric(sapply(away_team, function(x)which(unique(c(names(standat$winner), names(standat$loser))) == x)))
  standat
}
