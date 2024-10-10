// obtain winning probabilities for a given k and set of start ratings
// winning probs are from the perspective of the actual winner
vector win_probs_presence_nodraws(
         vector start_ratings,
         real k,
         int n_int, // n interactions
         int n_ind, // n individuals
         array[] int winner, // winner index
         array[] int loser, // loser index
         matrix presence // presence matrix
         ) {

  vector[n_int] result;
  real addval;
  vector[n_ind] current_ratings = start_ratings;
  for (i in 1:n_int) {
    // center :
    current_ratings = current_ratings - dot_product(row(presence, i), current_ratings) / sum(row(presence, i));
    // likelihood:
    result[i] = 1/(1 + exp((current_ratings[loser[i]] - current_ratings[winner[i]])));
    // result[i] = inv_logit(current_ratings[loser[i]] - current_ratings[winner[i]]);
    // update:
    addval = (1 - result[i]) * k;
    current_ratings[winner[i]] = current_ratings[winner[i]] + addval;
    current_ratings[loser[i]] = current_ratings[loser[i]] - addval;
  }
  return result;
}
