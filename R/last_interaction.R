#' find last interaction date of an individual
#'
#' @param test_ids character vector with id codes
#' @param test_dates character or Date vector: dates to be tested (must be
#'        same length as \code{test_ids})
#' @param interaction_dates character or Date vector of observed interaction
#' @param winner,loser character vector with id codes of winners and losers
#'
#' @returns a character vector with dates
#' @export
#'
#' @examples
#' data(adv, package = "EloRating")
#' # g's first interactions:
#' adv$Date[adv$winner == "g" | adv$loser == "g"][1:4]
#' last_interaction("g", "2010-01-07", interaction_dates = adv$Date, winner = adv$winner, loser = adv$loser)
#' last_interaction("g", "2010-01-08", interaction_dates = adv$Date, winner = adv$winner, loser = adv$loser)
#' last_interaction("g", "2010-01-01", interaction_dates = adv$Date, winner = adv$winner, loser = adv$loser)
last_interaction <- function(test_ids, test_dates, interaction_dates, winner, loser) {
  if (length(test_ids) != length(test_dates)) stop("id and test dates vector do not match in length")
  v <- seq_along(test_ids)
  aux <- sapply(v, function(x){
    out <- as.character(interaction_dates)[interaction_dates <= test_dates[x] & (winner == test_ids[x] | loser == test_ids[x])]
    if (length(out) == 0) out <- NA
    out
  }, simplify = FALSE)
  unlist(lapply(aux, max))
}
