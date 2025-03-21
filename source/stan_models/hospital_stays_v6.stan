
data {
    int<lower=0> T;                // Number of dates  -- come back to this to make sure it matched the generated dates
    int<lower=0> N;                // Number of obs hospitalizations
    array[N] int<lower=0> incid_h_t; // individual's day of incident hospitalization
    // vector<lower=0>[N] incid_h_t; // individual's day of incident hospitalization
    //array[T] int<lower=0> census_h; // census hosp
    vector<lower=0>[T] census_h; // census hosp
    real<lower=0> los_prior; 
    int<lower=0> los_indiv[N];
}


transformed data {
    int<lower=0> end_hosp_t[N]; // individual last day of hospitalization
    for (i in 1:N) {
        end_hosp_t[i] = incid_h_t[i] + los_indiv[i] - 1;
    }
}

parameters {
    real<lower=0> los_mean;  // Length of stay parameter to optimize
    // vector<lower=0>[T] census_h_calc; // list of census days calculated
    
}

// *check what goes in the transformed parameters vs transformed data
// (Transformed) Parameters cannot be integers.
transformed parameters {
    
    real<lower=0> neg_binom_alpha;
    // vector<lower=0>[N] los_indiv;
    // vector<lower=0>[N] end_hosp_t; // individual last day of hospitalization

    vector[T] t_zeros = rep_vector(0, T);
    vector[T] census_h_calc = rep_vector(0, T);

    for (i in 1:N) {
        vector[T] census_tmp = t_zeros;
        for (j in incid_h_t[i]:end_hosp_t[i]){
            census_tmp[j] = 1;
        }
        census_h_calc = census_h_calc + census_tmp;
    }

    neg_binom_alpha = los_mean * 0.5; // Why is this multiplied by 0.5? I dont remember. shoudl figure out and make a note
    // oh, i think it might be to transform the mean to the alpha parameter of the neg binomial distribution, but need to check this is correct

}


model{
    
    // probably need to add a prior on los_mean
    los_mean ~ normal(los_prior, 2); // prior on length of stay
    // los_indiv ~ poisson(los_mean, 0.5); // prior on length of stay
    los_indiv ~ neg_binomial(neg_binom_alpha, 0.5); // prior on length of stay

    census_h ~ normal(census_h_calc, 0.5); // prior on length of stay
    // census_h ~ normal(census_h_calc, 1); // prior on length of stay
}




