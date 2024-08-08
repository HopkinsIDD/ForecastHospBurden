
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
    // // update function name so rng is allowed in the function    
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
            // 
            // vector[] covidhosp_census_funct(int N, int T, int[] incid_h_t, int[] end_hosp_t) { 
                // 
                //     vector[T] census_h_calc ;  
                //     
                //     // fill in cencus_h_calc with 0s
                //     for (i in 1:T) {
                    //         census_h_calc[i] = 0;
                    //     }
                    //     
                    //     for (i in 1:T) {
                        //         census_h_calc[i] = num_matches(incid_h_t, end_hosp_t, i);
                        //     }
                        //     return census_h_calc;
                        // }
}

data {
    int<lower=0> T;                // Number of dates  -- come back to this to make sure it matched the generated dates
    int<lower=0> N;                // Number of obs hospitalizations
    //array[N] int<lower=0> incid_h_t; // individual's day of incident hospitalization 
    int<lower=0> incid_h_t[N]; // individual's day of incident hospitalization
    int<lower=0> end_hosp_t[N]; // individual's day of incident hospitalization
    //array[T] int<lower=0> census_h; // census hosp
    int<lower=0> census_h[T]; // census hosp
    int<lower=0> los_prior; 
    
}


transformed data {
    
    // array[N] int<lower=0> end_hosp_t; // individual last day of hospitalization
    // // end_hosp_t = calc_hosp_end_t_rng(N, los_mean, incid_h_t);
    // for (n in 1:N) {
        //     int los_calc;
        //     los_calc = neg_binomial_rng(los_mean, 0.5);
        //     end_hosp_t[n] = los_calc + incid_h_t[n] - 1; 
        // }
        // 
        // array[T] int<lower=0> census_h_calc; // list of census days calculated
        // census_h_calc = covidhosp_census_funct(N, T, incid_h_t, end_hosp_t); // list of hospitalizations
        
        // create a new census_h_new from census_h that is the same length as the calculated census_h_calc
        // fill in the values of census_h_new that are greater than the length of census_h with 0
        // this is to make sure that the two arrays are the same length
        
        // *moved all to transformed parameters since end_hosp_t is used in both and end_hosp_t depends on los_mean
        
        //int<lower=0> max_end_hosp; 
        //max_end_hosp = max(end_hosp_t);
}

parameters {
    real<lower=0> los_mean;  // Length of stay parameter to optimize
    // vector<lower=0>[T] census_h_calc; // list of census days calculated
    
}

// *check what goes in the transformed parameters vs transformed data
// (Transformed) Parameters cannot be integers.
transformed parameters {
    
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




