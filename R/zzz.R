#' a toy data set
#'
#' @aliases toyset toydata
#' @examples
#' toyres <- toydata()
#'
#' \dontrun{
#' set.seed(123)
#' x <- EloRating::randomsequence(nID = 12, avgIA = 27, presence = c(0.4, 0.4))
#' winner <- x$seqdat$winner
#' loser <- x$seqdat$loser
#' Date <- x$seqdat$Date
#' presence <- x$pres
#' standat <- prep_seq(winner = winner, loser = loser, Date = Date, presence = presence)
#' toyress <- elo_seq_bayes(standat = standat, quiet = FALSE, parallel_chains = 4, seed = 1)
#' }
#'
#' @export

toydata <- function() {
  d1 <- readRDS(file.path(system.file(package="EloRating.Bayes"), "extdata/toyset/toyset_modres.rds"))
  d2 <- readRDS(file.path(system.file(package="EloRating.Bayes"), "extdata/toyset/toyset_standat.rds"))

  res <- list(mod_res = d1, standat = d2)
  class(res) <- "bayesianelo"
  res
}

#' helpers
chaind_ids <- function(res) {
  rep(res$metadata()$id, each = res$metadata()$iter_sampling)
}
