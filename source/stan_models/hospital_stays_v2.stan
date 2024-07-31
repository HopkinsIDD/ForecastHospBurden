
functions {

    // generate hospital stays for N patients

    
    int num_matches(int[] x, int[] y, int a) {
        int n = 0;
        for (i in 1:size(x)){
            if (a >= x[i] & a <= y[i]){
              n += 1;
            }
        }
        return n;
    }
    
    int[] calc_hosp_end_t(int N, real los_mean, int[] incid_h_t) {) { 
        
        array[N] int<lower=0> end_hosp_t; // individual last day of hospitalization
        for (n in 1:N) {
            int los_calc;
            los_calc = neg_binomial_rng(los_mean, 0.5);
            end_hosp_t[n] = los_calc + incid_h_t[n] - 1; 
        }
        return end_hosp_t;
    }
    
    int[] covidhosp_census_funct(int N, int T2, real los_mean, int[] incid_h_t) {) { 

        array[N] int<lower=0> end_hosp_t; // individual last day of hospitalization
        end_hosp_t = calc_hosp_end_t(N, los_mean, incid_h_t);
        
        int<lower=0> max_end_hosp; 
        max_end_hosp = max(end_hosp_t);

        int<lower=0> I;
        
        // create I integer that is the max of T2 and the empirical max of end_hosp_t [FIX THIS]
        I = max(max_end_hosp, T2); // add empirical max to this
        array[I]<lower=0> int census_h_calc;
        
        // fill in cencus_h_calc with 0s
        for (i in 1:I) {
            census_h_calc[i] = 0;
        }
        
        census_h_calc = 0;
        for (i in 1:I) {
            census_h_calc[i] = num_matches(incid_h_t, end_hosp_t, i)
        }
        return census_h_calc;
    }
}

data {
    int<lower=0> T2;                // Number of dates  -- come back to this to make sure it matched the generated dates
    int<lower=0> N;                // Number of obs hospitalizations
    array[N] int<lower=0> incid_h_t; // individual's day of incident hospitalization 
    array[T] int<lower=0> census_h; // census hosp 
    int<lower=0> los_prior; 
}

parameters {
    real<lower=0> los_mean;  // Length of stay parameter to optimize
}

transformed parameters {
    array[T] int<lower=0> census_h_calc;
    census_h_calc = covidhosp_census_funct(N, los_mean, incid_h_t); // list of hospitalizations
}

transformed data {
    
    // create a new census_h_new from census_h that is the same length as the calculated census_h_calc
    // fill in the values of census_h_new that are greater than the length of census_h with 0
    // this is to make sure that the two arrays are the same length
    
    // fix this to use the right value for T (maybe T2?)
    array[T] int<lower=0> census_h_new;
    for (i in 1:T) {
        if (i <= N) {
            census_h_new[i] = census_h[i];
        } else {
            census_h_new[i] = 0;
        }
    }
}

model{
    
    // probably need to add a prior on los_mean
    
    los_mean ~ normal(los_prior, 2); // prior on length of stay
    target += neg_binomial_lpmf(cencus_h | census_h_calc, 0.5); // prior on length of stay
    
    // should we rewrite this to minimize the log-likelihood?
}

// generated quantities {
//   // Generate hospital stays for N patients
//   int hospital_stays[N];
//   
//   # call function, generate hosp stays 
//   hospital_stays = covidhosp_stay_funct(N, los);
// 
// }