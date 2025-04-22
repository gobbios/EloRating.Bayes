// code is heavily based on Goffe et al 2018

functions {
  #include win_probs_presence_draws.stan
  #include ratings_up_to.stan
}

data {
  int<lower=1> n_ind; // number of individuals
  int<lower=1> n_int; // number of interactions
  int<lower=1> n_k; // number of interaction types/intensities
  array[n_int] int<lower=1> winner_index; // winner's index
  array[n_int] int<lower=1> loser_index; // losers's index
  array[n_int] int draws; // draw? 1=yes, 0=no
  array[n_int] int<lower=1,upper=n_k> intensity_index; // interaction type/intensity index
  matrix[n_int, n_ind] presence; // presence matrix
  int<lower=1> n_extract; // extraction points
  array[n_extract] int<lower=1> targetdates; // index positions for extraction

  // conditional estimation of SD of start values
  int<lower = 0, upper = 1> startspread_fixed; // toggle estimation of start SD
  // startspread_val is size 0 if startspread_fixed is FALSE/0
  // otherwise, we hand a value over in the data list
  array[startspread_fixed ? 1 : 0] real<lower=0> startspread_val;
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
  // real<lower=0.0> k;
  vector<lower=0>[n_k] k;
  // startspread_param is size 0 if startspread_fixed is TRUE/1
  array[startspread_fixed ? 0 : 1] real<lower=0> startspread_param;
}

transformed parameters {
  // either use data value or estimate
  real<lower=0> startspread;
  if (startspread_fixed) {
    startspread = startspread_val[1];
  } else {
    startspread = startspread_param[1];
  }
}

model {
  // elo part
  k ~ exponential(2);
  EloStart ~ normal(0, startspread);
  if (startspread_fixed) {
    startspread ~ exponential(1);
  }
  y ~ bernoulli(win_probs_presence_draws(EloStart, k, n_int, n_ind, winner_index, loser_index, presence, draws, intensity_index));
}

generated quantities {
  // generate the actual ratings at the required dates
  matrix[n_extract, n_ind] out_perdate;
  vector[n_int] gq_winprobs;
  array[n_int] int gq_int_outcome;

  gq_winprobs = win_probs_presence_draws(EloStart, k, n_int, n_ind, winner_index, loser_index, presence, draws, intensity_index);
  gq_int_outcome = bernoulli_rng(gq_winprobs);
  for (i in 1:n_extract) {
    out_perdate[i, ] = to_row_vector(ratings_up_to(EloStart, k, n_int, n_ind, winner_index, loser_index, targetdates[i], presence, draws, intensity_index));
  }
}

