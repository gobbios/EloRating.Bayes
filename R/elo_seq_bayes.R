#' Elo-rating with Bayesian flavor
#'
#' @param standat output of \code{\link{prep_seq}}
#' @param quiet logical, default is \code{FALSE}. If set to \code{TRUE}:
#'              try to swallow all output cmdstanr prints
#'              to the console (which can be annoying in some contexts).
#' @param ... additional arguments to \code{cmdstanr}'s
#'            \code{\link[cmdstanr]{sample}} method (typically
#'            \code{parallel_chains}, \code{refresh},
#'            \code{iter_warmup}, \code{iter_sampling},
#'            \code{seed}, \code{adapt_delta},
#'            and/or \code{step_size})
#'
#' @references
#' The underlying is Stan code is heavily based on this paper:
#'
#' \insertRef{goffe2018}{EloRating.Bayes}
#'
#' @importFrom cmdstanr cmdstan_model
#' @importFrom Rdpack reprompt
#' @importFrom EloRating randomsequence
#'
#' @return a list with two items, where the first (\code{$mod_res}) is the
#'         the \code{cmdstanr} model object (see
#'         \code{\link[cmdstanr]{CmdStanModel}}). The second list item
#'         (\code{$standat}) is the list supplied as stan data (which
#'         is required for further processing).
#' @export
#'
#' @examples
#' library(EloRating)
#' x <- randomsequence(nID = 7, avgIA = 30)$seqdat
#' standat <- prep_seq(winner = x$winner, loser = x$loser, Date = x$Date)
#' res <- elo_seq_bayes(standat = standat, refresh = 0, parallel_chains = 2)
#' summary(res)
#'
#' # incorporating presence and ties
#' x <- randomsequence(nID = 8, avgIA = 20, presence = c(0.2, 0.4), ties = 0.3)
#' winner <- x$seqdat$winner
#' loser <- x$seqdat$loser
#' Date <- x$seqdat$Date
#' presence <- x$pres
#' draws <- x$seqdat$Draw
#' standat <- prep_seq(winner = winner, loser = loser, Date = Date,
#'                     presence = presence, draws = draws)
#' res <- elo_seq_bayes(standat = standat, refresh = 0, chains = 3, iter_sampling = 333)
#' summary(res)


elo_seq_bayes <- function(standat, quiet = FALSE, ...) {
  # path to model file
  f <- system.file("extdata/elo_withdates.stan", package = "EloRating.Bayes")
  # compile model file and sample
  if (quiet) {
    x <- capture.output(mod <- suppressMessages(cmdstan_model(f, quiet = TRUE)))
    x <- capture.output(res <- suppressMessages(mod$sample(data = standat, ...)))
  } else {
    mod <- cmdstan_model(f, quiet = TRUE)
    res <- mod$sample(data = standat, ...)
  }

  res <- list(mod_res = res, standat = standat)
  class(res) <- "bayesianelo"
  res
}
