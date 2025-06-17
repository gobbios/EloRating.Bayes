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
#' @importFrom stats sd
#'
#' @return a list with two items, where the first (\code{$mod_res}) is the
#'         the \code{cmdstanr} model object (see
#'         \code{\link[cmdstanr]{CmdStanModel}}). The second list item
#'         (\code{$standat}) is the list supplied as stan data (which
#'         is required for further processing).
#' @export
#'
#' @examples
#' x <- EloRating::randomsequence(nID = 7, avgIA = 30)$seqdat
#' standat <- prep_seq(winner = x$winner, loser = x$loser, Date = x$Date)
#' res <- elo_seq_bayes(standat = standat, refresh = 0, parallel_chains = 2)
#' summary(res)
#'
#' # incorporating presence and ties
#' x <- EloRating::randomsequence(nID = 8, avgIA = 20,
#'                                presence = c(0.2, 0.4), ties = 0.3)
#' winner <- x$seqdat$winner
#' loser <- x$seqdat$loser
#' Date <- x$seqdat$Date
#' presence <- x$pres
#' draws <- x$seqdat$Draw
#' standat <- prep_seq(winner = winner, loser = loser, Date = Date,
#'                     presence = presence, draws = draws,
#'                     estimate_startspread = TRUE)
#' res <- elo_seq_bayes(standat = standat, refresh = 0, chains = 3,
#'                      iter_sampling = 333)
#' summary(res)
#'
#' x <- EloRating::randomsequence(nID = 8, avgIA = 20)$seqdat
#' x$intensity <- sample(c("contact", "mild"), nrow(x), TRUE)
#' standat <- prep_seq(winner = x$winner, loser = x$loser, Date = x$Date,
#'                     intensity = x$intensity)
#' res <- elo_seq_bayes(standat = standat, refresh = 0, parallel_chains = 2)
#' summary(res)
#'
#' # use pre-defined extract/target dates
#' data(adv, package = "EloRating")
#' data(advpres, package = "EloRating")
#' standat <- prep_seq(winner = adv$winner, loser = adv$loser, Date = adv$Date,
#'                     presence = advpres, extract_dates = c("2010-01-01", "2010-01-11", "2010-01-21"))
#' res <- elo_seq_bayes(standat = standat, refresh = 0, parallel_chains = 2)
#' # and extract summaries at specified dates
#' # (currently ignoring presence)
#' res$post_summaries



elo_seq_bayes <- function(standat, quiet = FALSE, ...) {
  # path to model file
  f <- system.file("extdata/elo_withdates.stan", package = "EloRating.Bayes")
  # path for including standalone functions
  includepath <- normalizePath(file.path(system.file("extdata", package = "EloRating.Bayes"), "stan_functions"))

  # compile model file and sample
  if (quiet) {
    x <- capture.output(mod <- suppressMessages(cmdstan_model(f, quiet = TRUE, include_paths = includepath)))
    x <- capture.output(res <- suppressMessages(mod$sample(data = standat, ...)))
  } else {
    mod <- cmdstan_model(f, quiet = TRUE, include_paths = includepath)
    res <- mod$sample(data = standat, ...)
  }
  post <- res$draws("out_perdate", format = "draws_matrix")
  post_summaries <- data.frame(id = rep(colnames(standat$presence),  each = standat$n_extract),
                               date = rep(names(standat$targetdates), standat$n_ind)
                               )
  post_summaries$mean <- colMeans(post)
  post_summaries$median <- apply(post, 2, median)
  post_summaries$sd <- apply(post, 2, sd)
  post_summaries$l50 <- apply(post, 2, quantile, probs = 0.25)
  post_summaries$u50 <- apply(post, 2, quantile, probs = 0.75)
  post_summaries$l89 <- apply(post, 2, quantile, probs = 0.055)
  post_summaries$u89 <- apply(post, 2, quantile, probs = 0.945)
  post_summaries$l95 <- apply(post, 2, quantile, probs = 0.025)
  post_summaries$u95 <- apply(post, 2, quantile, probs = 0.975)

  res <- list(mod_res = res, standat = standat, post_summaries = post_summaries)

  class(res) <- "bayesianelo"
  res
}
