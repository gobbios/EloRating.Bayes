// obtain winning probabilities for a given k and set of start ratings
// winning probs are from the perspective of the actual winner
vector win_probs_presence_nodraws_homeadvantage(
         vector start_ratings,
         real k,
         int n_int, // n interactions
         int n_ind, // n individuals
         array[] int hometeam_index, // hometeam index
         array[] int awayteam_index, // awayteam index
         matrix presence, // presence matrix
         real homeadv_mean,
         vector homeadv_blups,
         vector homewin
         ) {

  vector[n_int] winprob_home = rep_vector(0.0, n_int);
  real addval;
  vector[n_ind] current_ratings = start_ratings;
  vector[n_ind] homestrength_vec = homeadv_blups + homeadv_mean;

  for (i in 1:n_int) {
    // center :
    current_ratings = current_ratings - dot_product(row(presence, i), current_ratings) / sum(row(presence, i));
    // likelihood:
    // winprob_home[i] = 1/(1 + exp((current_ratings[loser[i]] - current_ratings[winner[i]])));
    // winprob_home[i] = inv_logit(current_ratings[loser[i]] - current_ratings[winner[i]]);
    winprob_home[i] = inv_logit((current_ratings[hometeam_index[i]] + homestrength_vec[hometeam_index[i]]) -
                                  current_ratings[awayteam_index[i]]);
    // update:
    if (homewin[i] == 1.0) { // home team won
      addval = (1 - winprob_home[i]) * k; // fix k = 0.0
      current_ratings[hometeam_index[i]] = current_ratings[hometeam_index[i]] + addval;
      current_ratings[awayteam_index[i]] = current_ratings[awayteam_index[i]] - addval;
    }

    if (homewin[i] == 0.0) { // home team lost
      addval = (1 - winprob_home[i]) * k; // fix k = 0.0
      current_ratings[hometeam_index[i]] = current_ratings[hometeam_index[i]] - addval;
      current_ratings[awayteam_index[i]] = current_ratings[awayteam_index[i]] + addval;
    }

  }
  return winprob_home;
}

