
functions {

    // generate hospital stays for N patients

    int num_matches(int[] x, int[] y, int a) {
        int n = 0;
        for (i in 1:size(x)){
            if (a >= x[i] && a <= y[i]){
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
        // move to different block so can be read in with T2
        //array[N] int<lower=0> end_hosp_t; // individual last day of hospitalization
        //end_hosp_t = calc_hosp_end_t(N, los_mean, incid_h_t);
        
        
        // create I integer that is the max of T2 and the empirical max of end_hosp_t [FIX THIS]
        //int<lower=0> I;
        //I = max(max_end_hosp, T2); // add empirical max to this
        //array[I]<lower=0> int census_h_calc;
        
        // replacing I with T2 defined as the max of T and the empirical max of end_hosp_t
        array[T2]<lower=0> int census_h_calc;
        
        // fill in cencus_h_calc with 0s
        for (i in 1:T2) {
            census_h_calc[i] = 0;
        }
        
        census_h_calc = 0;
        for (i in 1:T2) {
            census_h_calc[i] = num_matches(incid_h_t, end_hosp_t, i)
        }
        return census_h_calc;
    }
}

data {
    int<lower=0> T;                // Number of dates  -- come back to this to make sure it matched the generated dates
    int<lower=0> N;                // Number of obs hospitalizations
    array[N] int<lower=0> incid_h_t; // individual's day of incident hospitalization 
    array[T] int<lower=0> census_h; // census hosp 
    int<lower=0> los_prior; 
}

parameters {
    real<lower=0> los_mean;  // Length of stay parameter to optimize
}

// *check what goes in the transformed parameters vs transformed data
transformed parameters {
    array[T] int<lower=0> census_h_calc;
    census_h_calc = covidhosp_census_funct(N, los_mean, incid_h_t); // list of hospitalizations
    //census_h_calc = covidhosp_census_funct(N, T, los_mean, incid_h_t); // list of hospitalizations
    
    array[N] int<lower=0> end_hosp_t; // individual last day of hospitalization
    end_hosp_t = calc_hosp_end_t(N, los_mean, incid_h_t);
    
    int<lower=0> max_end_hosp; 
    max_end_hosp = max(end_hosp_t);

    // fix this to use the right value for T (maybe T2?)
    // define T2 so it's the max of T (dates) and the max of end_hosp_t (empirical max of end_hosp_t
    int T2 = max(T, max(end_hosp_t));
    
    // is it better to trim census_h_calc, so it doesn't go beyond observed dates?
    // vs. adding in 0's for "oberseved" hosp dates that are beyond the end of the data?
    array[T2] int<lower=0> census_h_new;
    for (i in 1:T2) {
        if (i <= N) {
            census_h_new[i] = census_h[i];
        } else {
            census_h_new[i] = 0;
        }
    }
}

//transformed data {
    // create a new census_h_new from census_h that is the same length as the calculated census_h_calc
    // fill in the values of census_h_new that are greater than the length of census_h with 0
    // this is to make sure that the two arrays are the same length
    
    // *moved all to transformed parameters since end_hosp_t is used in both and end_hosp_t depends on los_mean
    
    //int<lower=0> max_end_hosp; 
    //max_end_hosp = max(end_hosp_t);

    // fix this to use the right value for T (maybe T2?)
    // define T2 so it's the max of T (dates) and the max of end_hosp_t (empirical max of end_hosp_t
    //int T2 = max(T, max(end_hosp_t));
    
    //array[T2] int<lower=0> census_h_new;
    //for (i in 1:T2) {
        //if (i <= N) {
            //census_h_new[i] = census_h[i];
        //} else {
            //census_h_new[i] = 0;
        //}
    //}
//}

model{
    
    // probably need to add a prior on los_mean
    
    los_mean ~ normal(los_prior, 2); // prior on length of stay
    target += neg_binomial_lpmf(census_h_new | census_h_calc, 0.5); // prior on length of stay
    
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