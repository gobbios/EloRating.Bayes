
set.seed(123)
x <- EloRating::randomsequence(nID = 6, avgIA = 10, presence = c(0.4, 0.4))
winner <- x$seqdat$winner
loser <- x$seqdat$loser
Date <- x$seqdat$Date
presence <- x$pres
intensity <- sample(c("mild", "severe"), length(winner), TRUE)
standat <- prep_seq(winner = winner, loser = loser, Date = Date, presence = presence, intensity = intensity)
toyset <- elo_seq_bayes(standat = standat, quiet = FALSE, parallel_chains = 4, seed = 1)


toyset$mod_res$save_object("inst/extdata/toyset/toyset_modres.rds")
toyset_standat <- toyset$standat
saveRDS(toyset_standat, "inst/extdata/toyset/toyset_standat.rds")

