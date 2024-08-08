
functions {
    
    // generate hospital stays for N patients
    int num_matches(int[] x, int[] y, int a){
        int n = 0;
        for (i in 1:size(x)){
            if (a >= x[i] && a <= y[i]){
                n += 1;
            }
        }
        return n;
    }
}

data {
    int<lower=0> T;                // Number of dates  -- come back to this to make sure it matched the generated dates
    int<lower=0> N;                // Number of obs hospitalizations
    //array[N] int<lower=0> incid_h_t; // individual's day of incident hospitalization 
    int<lower=0> incid_h_t[N]; // individual's day of incident hospitalization
    //array[T] int<lower=0> census_h; // census hosp
    int<lower=0> census_h[T]; // census hosp
    int<lower=0> los_prior; 
    
}


transformed data {
    
    // int<lower=0> end_hosp_t[N]; // individual last day of hospitalization
    // for (n in 1:N) {
    //     int los_calc;
    //     los_calc = neg_binomial_rng(los_mean, 0.5);
    //     end_hosp_t[n] = los_calc + incid_h_t[n] - 1; 
    // }
    // 
}

parameters {
    real<lower=0> los_mean;  // Length of stay parameter to optimize
    // vector<lower=0>[T] census_h_calc; // list of census days calculated
    
}

// *check what goes in the transformed parameters vs transformed data
// (Transformed) Parameters cannot be integers.
transformed parameters {
    
    vector<lower=0>[T] end_hosp_t; // individual last day of hospitalization
    for (n in 1:N) {
        int los_calc;
        los_calc = neg_binomial_rng(los_mean, 0.5);
        end_hosp_t[n] = los_calc + incid_h_t[n] - 1; 
    }
    
    vector<lower=0>[T] census_h_calc; // list of census days calculated
    // census_h_calc = covidhosp_census_funct(N, T, incid_h_t, end_hosp_t); // list of hospitalizations
    for (i in 1:T) {
        census_h_calc[i] = num_matches(incid_h_t, end_hosp_t, i);
    }
}

model{
    
    // probably need to add a prior on los_mean
    los_mean ~ normal(los_prior, 2); // prior on length of stay
    target += neg_binomial_lpmf(census_h | census_h_calc, 0.5); // prior on length of stay
    // census_h ~ normal(census_h_calc, 1); // prior on length of stay
}




