#' summarize bayesianelo object
#'
#' @param object an object of class \code{"bayesianelo"},
#'               usually the result of a call to \code{\link{elo_seq_bayes}}
#' @param ... further arguments passed to or from other methods (ignored)
#' @importFrom stats median quantile
#' @author Christof Neumann
#'
#' @export

summary.bayesianelo <- function(object, ...) {
  xtab <- table(c(names(object$standat$winner_index), names(object$standat$loser_index)))
  d <- object$mod_res$draws("k", format = "draws_matrix")
  qs <- sprintf("%.3f", quantile(d, c(0.055, 0.5, 0.945)))
  diagnostics <- object$mod_res$diagnostic_summary()
  if (all(diagnostics$num_divergent == 0) &
      all(diagnostics$num_max_treedepth == 0) &
      all(diagnostics$ebfmi > 0.2)) {
    diags <- TRUE
    msg <- "no obvious sampling issues detected"
  } else {
    diags <- FALSE
    msg <- "sampling issues detected (see below)"
  }

  cat("Bayesian Elo ratings from", object$standat$n_ind, "individuals\n")
  cat("total (mean/median) number of interactions: ",
      object$standat$n_int, " (",
      sprintf("%.1f", mean(xtab)), "/",
      sprintf("%.1f", median(xtab)), ")\n", sep = "")
  cat("range of interactions:", min(xtab), "-", max(xtab), "\n")
  cat("date range:",
      as.character(min(as.Date(names(object$standat$idates)))), "-",
      as.character(max(as.Date(names(object$standat$idates)))), "\n")
  # cat("startvalue:", "NA", "\n")
  # cat("uppon arrival treatment:", "NA", "\n")
  cat("median k (89% CI): ", qs[2], " (", qs[1], " - ", qs[3], ")\n", sep = "")
  cat("proportion of draws in the data set:", sprintf("%.2f", mean(object$standat$draws)), "\n")
  cat("number of post-warmup samples:", length(object$mod_res$metadata()$id) * object$mod_res$metadata()$iter_sampling, "\n")
  if (diags) cat(msg, "\n")
  if (!diags) {
    cat(msg, "\n")
    if (interactive()) {
      print(diagnostics)
    }
  }
  cat("\n")

}
