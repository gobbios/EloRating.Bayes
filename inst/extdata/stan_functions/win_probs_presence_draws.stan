// obtain winning probabilities for a given k and set of start ratings
// k is a vector depending on different intensities/interaction types...
vector win_probs_presence_draws(
         vector start_ratings,
         vector k,
         int n_int, // n interactions
         int n_ind, // n individuals
         array[] int winner, // winner index
         array[] int loser, // loser index
         matrix presence, // , // presence matrix
         array[] int draw, // indicator for draws
         array[] int intensity // index for k vector
         ) {

  vector[n_int] result;
  real addval;
  vector[n_ind] current_ratings = start_ratings;
  for (i in 1:n_int) {
    // center :
    current_ratings = current_ratings - dot_product(row(presence, i), current_ratings) / sum(row(presence, i));
    // likelihood:
    result[i] = 1/(1 + exp((current_ratings[loser[i]] - current_ratings[winner[i]])));
    // update:
    if (draw[i]) {
      addval = (1 - result[i]) * k[intensity[i]];
      current_ratings[winner[i]] = current_ratings[winner[i]] + addval - k[intensity[i]] * 0.5;
      current_ratings[loser[i]] = current_ratings[loser[i]] - addval + k[intensity[i]] * 0.5;
    } else {
      addval = (1 - result[i]) * k[intensity[i]];
      current_ratings[winner[i]] = current_ratings[winner[i]] + addval;
      current_ratings[loser[i]] = current_ratings[loser[i]] - addval;
    }
  }
  return result;
}
