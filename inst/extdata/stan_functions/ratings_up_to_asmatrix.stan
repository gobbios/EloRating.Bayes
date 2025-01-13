// ratings for multiple up-to 'dates'
// important: stop_here has to be increasing!!!
matrix ratings_up_to_asmatrix(
         vector start_ratings,
         vector k,
         int n_int, // n interactions
         int n_ind, // n individuals
         array[] int winner, // winner index
         array[] int loser, // loser index
         array[] int stop_here, // index of extraction positions ('dates'): must be increasing!!!
         matrix presence, // presence matrix
         array[] int draw, // indicator for draws
         array[] int intensity // index for k vector
         ) {

    vector[n_int] result;
    real addval;
    row_vector[n_ind] current_ratings = to_row_vector(start_ratings);
    matrix[size(stop_here), n_ind] out = rep_matrix(0.0, size(stop_here), n_ind);
    int target_index = 1;

    for (i in 1:max(stop_here)) {
      // centering:
      current_ratings = current_ratings - dot_product(row(presence, i), current_ratings) / sum(row(presence, i));
      // likelihood:
      // result[i] = 1/(1 + exp((current_ratings[loser[i]] - current_ratings[winner[i]])));
      result[i] = inv_logit(current_ratings[winner[i]] - current_ratings[loser[i]]); // winner-loser reversed with inv_logit!!!
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
      if (i == stop_here[target_index]) {
        out[target_index, ] = current_ratings;
        target_index = target_index + 1;
      }
    }
    return out;
  }
