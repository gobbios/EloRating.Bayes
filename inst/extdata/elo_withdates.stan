// code is heavily based on Goffe et al 2018

functions {
  // obtain winning probabilities for a given k and set of start ratings
  vector win_probs_from_seq(vector start_ratings,
                            real k,
                            int N, // n interaction
                            int K, // n individuals
                            array[] int winner, // winner index
                            array[] int loser, // loser index
                            matrix presence, // presence matrix
                            array[] int draw // indicator for draws
                            ) {
    vector[N] result;
    real toAdd;
    vector[K] current_ratings = start_ratings;
    for (i in 1:N) {
      // center :
      current_ratings = current_ratings - dot_product(row(presence, i), current_ratings) / sum(row(presence, i));
      // likelihood:
      result[i] = 1/(1 + exp((current_ratings[loser[i]] - current_ratings[winner[i]])));
      // update:
      if (draw[i]) {
        toAdd = (1 - result[i]) * k;
        current_ratings[winner[i]] = current_ratings[winner[i]] + toAdd - k * 0.5;
        current_ratings[loser[i]] = current_ratings[loser[i]] - toAdd + k * 0.5;
      } else {
        toAdd = (1 - result[i]) * k;
        current_ratings[winner[i]] = current_ratings[winner[i]] + toAdd;
        current_ratings[loser[i]] = current_ratings[loser[i]] - toAdd;
      }
    }
    return result;
  }

  // obtain ratings at the point of a given interaction ('date')
  vector ratings_up_to(vector start_ratings,
                            real k,
                            int N, // n interaction
                            int K, // n individuals
                            array[] int winner, // winner index
                            array[] int loser, // loser index
                            int stop_here, // index of extraction position ('date')
                            matrix presence, // presence matrix
                            array[] int draw // indicator for draws
                            ) {
    vector[N] result;
    real toAdd;
    vector[K] current_ratings = start_ratings;

    for (i in 1:stop_here) {
      // centering:
      current_ratings = current_ratings - dot_product(row(presence, i), current_ratings) / sum(row(presence, i));
      // likelihood:
      result[i] = 1/(1 + exp((current_ratings[loser[i]] - current_ratings[winner[i]])));
      // update:
      if (draw[i]) {
        toAdd = (1 - result[i]) * k;
        current_ratings[winner[i]] = current_ratings[winner[i]] + toAdd - k * 0.5;
        current_ratings[loser[i]] = current_ratings[loser[i]] - toAdd + k * 0.5;
      } else {
        toAdd = (1 - result[i]) * k;
        current_ratings[winner[i]] = current_ratings[winner[i]] + toAdd;
        current_ratings[loser[i]] = current_ratings[loser[i]] - toAdd;
      }
    }
    return current_ratings;
  }

}

data {
  int<lower=1> n_ind; // number of individuals
  int<lower=1> n_int; // number of interactions
  array[n_int] int<lower=1> winner_index; // winner's index
  array[n_int] int<lower=1> loser_index; // losers's index
  array[n_int] int draws; // draw? 1=yes, 0=no
  matrix[n_int, n_ind] presence; // presence matrix
  int<lower=1> n_extract; // extraction points
  array[n_extract] int<lower=1> targetdates; // index positions for extraction
}

transformed data {
  // create the response variable, which is simply a vector of 1's
  array[n_int] int<lower=0> y;
  for (i in 1:n_int) {
    y[i] = 1;
  }
}

parameters {
  vector[n_ind] EloStart;
  real<lower=0.0> k;
}

model {
  // elo part
  k ~ exponential(2);
  EloStart ~ normal(0, 1);

  y ~ bernoulli(win_probs_from_seq(EloStart, k, n_int, n_ind, winner_index, loser_index, presence, draws));
}

generated quantities {
  // generate the actual ratings at the required dates
  matrix[n_extract, n_ind] out_perdate;

  for (i in 1:n_extract) {
    out_perdate[i, ] = to_row_vector(ratings_up_to(EloStart, k, n_int, n_ind, winner_index, loser_index, targetdates[i], presence, draws));
  }
}

