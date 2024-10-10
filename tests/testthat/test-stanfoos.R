library(cmdstanr)

scode <- "
functions {
  #include ratings_up_to.stan
  #include ratings_up_to_asmatrix.stan
  #include win_probs_presence_nodraws.stan
  #include win_probs_presence_draws.stan
  #include win_probs_presence_nodraws_homeadvantage.stan
}
"

includepath <- normalizePath("../../inst/extdata/stan_functions", mustWork = FALSE)
if (!dir.exists(includepath)) {
  includepath <- normalizePath("~/Documents/projects_local/EloRating.Bayes/inst/extdata/stan_functions",
                               mustWork = FALSE)
}

m <- cmdstan_model(write_stan_file(scode),
                   # include_paths = c("~/Documents/projects_local/EloRating.Bayes/inst/extdata/stan_functions"),
                   include_paths = includepath,
                   force_recompile = TRUE)

m$expose_functions(global = TRUE)

# test data set
start_ratings <- rep(0, 6)
k <- 1
n_int <- 9
n_ind <- length(start_ratings)
winner <- c(1, 3, 1, 2, 2, 2, 5, 6, 1)
loser <- c(2, 4, 2, 1, 3, 4, 6, 5, 2)
draws <- rep(0, n_int)
presence <- matrix(ncol = n_ind, nrow = n_int, 1)

test_that("ratings_up_to works", {
  onego <- ratings_up_to_asmatrix(start_ratings = start_ratings, k = k, n_int = n_int, n_ind = n_ind,
                                  winner = winner, loser = loser, presence = presence, draw = draws,
                                  stop_here = c(1:9))
  onego <- round(onego, 3)
  for (i in 1:9) {
    r <- ratings_up_to(start_ratings = start_ratings, k = k, n_int = n_int, n_ind = n_ind,
                       winner = winner, loser = loser, presence = presence, draw = draws,
                       stop_here = i)
    r <- round(r, 3)
    expect_true(all(onego[i, ] == r))
  }


  xx <- randomsequence(nID = 10, avgIA = 15, ties = 0.2, presence = c(0.4, 0.3))
  xx <- prep_seq(winner = xx$seqdat$winner, loser = xx$seqdat$loser, Date = xx$seqdat$Date, presence = xx$pres, draws = xx$seqdat$Draw)

  onego <- ratings_up_to_asmatrix(start_ratings = rep(0, ncol(xx$presence)),
                                 k = 0.2,
                                 n_int = nrow(xx$presence),
                                 n_ind = ncol(xx$presence),
                                 winner = xx$winner_index,
                                 loser = xx$loser_index,
                                 presence = xx$presence,
                                 draw = xx$draws,
                                 stop_here = c(1:nrow(xx$presence)))
  onego <- round(onego, 3)
  for (i in seq_len(xx$n_int)) {
    r <- ratings_up_to(start_ratings = rep(0, ncol(xx$presence)),
                       k = 0.2,
                       n_int = nrow(xx$presence),
                       n_ind = ncol(xx$presence),
                       winner = xx$winner_index,
                       loser = xx$loser_index,
                       presence = xx$presence,
                       draw = xx$draws,
                       stop_here = i)
    r <- round(r, 3)
    expect_true(all(onego[i, ] == r))
  }



})



# winprobs ----
xx <- randomsequence(nID = 10, avgIA = 12, ties = 0.2, presence = c(0.4, 0.3))
xx <- prep_seq(winner = xx$seqdat$winner, loser = xx$seqdat$loser, Date = xx$seqdat$Date, presence = xx$pres, draws = xx$seqdat$Draw)

rats <- ratings_up_to_asmatrix(start_ratings = rep(0, ncol(xx$presence)),
                               k = 0.2,
                               n_int = nrow(xx$presence),
                               n_ind = ncol(xx$presence),
                               winner = xx$winner_index,
                               loser = xx$loser_index,
                               presence = xx$presence,
                               draw = xx$draws,
                               stop_here = c(1:nrow(xx$presence)))

probs <- win_probs_presence_draws(start_ratings = rep(0, ncol(xx$presence)),
                         k = 0.2,
                         n_int = nrow(xx$presence),
                         n_ind = ncol(xx$presence),
                         winner = xx$winner_index,
                         loser = xx$loser_index,
                         presence = xx$presence,
                         draw = xx$draws)
probs <- probs[-1]

res <- rep(0, xx$n_int - 1)
for (i in 2:(length(res))) {
  res[i - 1] <- plogis(rats[i - 1, xx$loser_index[i]] - rats[i - 1, xx$winner_index[i]])
}


test_that("winprobs add up...", {
  expect_true(all(round(res[-length(res)] - (1 - probs)[-length(res)], 12) == 0))
})










