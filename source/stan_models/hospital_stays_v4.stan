
functions {
    
    // generate hospital stays for N patients
    int num_matches(vector x, real[] y, int a){
        int n = 0;
        real x_;
        real y_;
        for (i in 1:size(x)){
                    x_ = x[i];
                    y_ = y[i];
            if (a >= x[i] && a <= y[i]){
                n += 1;
            }
        }
        return n;
    }
    
    //     // update function name so rng is allowed in the function    
    // int[] calc_hosp_end_t_rng(int N, real los_mean, int[] incid_h_t){ 
    //     
    //     //int[N] int<lower=0> end_hosp_t; // individual last day of hospitalization
    //     int end_hosp_t[N]; // individual last day of hospitalization, can't use a lower here? 
    //     for (n in 1:N) {
    //         int los_calc;
    //         los_calc = neg_binomial_rng(los_mean, 0.5);
    //         end_hosp_t[n] = los_calc + incid_h_t[n] - 1; 
    //     }
    //     return end_hosp_t;
    // }
}

data {
    int<lower=0> T;                // Number of dates  -- come back to this to make sure it matched the generated dates
    int<lower=0> N;                // Number of obs hospitalizations
    //array[N] int<lower=0> incid_h_t; // individual's day of incident hospitalization 
    vector<lower=0>[N] incid_h_t;
    //array[N] real<lower=0> incid_h_t; // individual's day of incident hospitalization
    //array[T] int<lower=0> census_h; // census hosp
    vector<lower=0>[T] census_h; // census hosp
    real<lower=0> los_prior; 
    
}


transformed data {
    

}

parameters {
    real<lower=0> los_mean;  // Length of stay parameter to optimize
    // vector<lower=0>[T] census_h_calc; // list of census days calculated
    
}

// *check what goes in the transformed parameters vs transformed data
// (Transformed) Parameters cannot be integers.

transformed parameters {
    
    real<lower=0> neg_binom_alpha;
    //array[N] real<lower=0> los_indiv; 
    vector<lower=0>[N] los_indiv;
    // census_h_calc = covidhosp_census_funct(N, T, incid_h_t, end_hosp_t); // list of hospitalizations
    array[N] real<lower=0> end_hosp_t; // individual last day of hospitalization
    array[T] real<lower=0> census_h_calc; // list of census days calculated
    
    for (i in 1:N) {
        end_hosp_t[i] = incid_h_t[i] + los_indiv[i] - 1;
    }
    for (i in 1:T) {
        census_h_calc[i] = num_matches(incid_h_t, end_hosp_t, i);
    }

    neg_binom_alpha = los_mean * 0.5;
}

model{
    
    // probably need to add a prior on los_mean
    los_mean ~ normal(los_prior, 2); // prior on length of stay
    // los_indiv ~ poisson(los_mean, 0.5); // prior on length of stay
    //los_indiv ~ neg_binomial(neg_binom_alpha, 0.5); // prior on length of stay
    los_indiv ~ gamma(neg_binom_alpha, 0.5); // gamma for now to allow continious value

    target += normal_lpdf(census_h | census_h_calc, 0.5); // prior on length of stay
    // census_h ~ normal(census_h_calc, 1); // prior on length of stay
}




