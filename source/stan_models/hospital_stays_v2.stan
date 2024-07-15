
functions {

    // generate hospital stays for N patients
    int[] covidhosp_stayend_funct(int N, real los_mean, int[] incid_h_t) {) { 
        int los_calc[N];
        int census_h_calc[N];
        los_calc = neg_binomial_rng(los_mean, 0.5);
        census_h_calc = los_calc + incid_h_t - 1; 
        
        return census_h_calc;
    }
    
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
    
    int[] covidhosp_census_funct(int N, int T, real los_mean, int[] incid_h_t) {) { 

        array[N] int<lower=0> end_hosp_t; // individual last day of hospitalization
        end_h_t = calc_hosp_end_t(N, los_mean, incid_h_t);
        
        int<lower=0> I;
        I = max(end_h_t);
        array[I]<lower=0> int census_h_calc;
        for (i in 1:I) {
            census_h_calc[i] = num_matches(incid_h_t, end_h_t, i)
        }
        return census_h_calc;
    }
}

data {
    int<lower=0> T;                // Number of dates 
    int<lower=0> N;                // Number of obs hospitalizations
    array[N] int<lower=0> incid_h_t; // individual day of incident hospitalization 
    array[T] int<lower=0> census_h; // census hosp 
}

parameters {
    real<lower=0> los_mean;  // Length of stay parameter to optimize
}
transformed parameters {
    census_h_calc = covidhosp_census_funct(N, T, los_mean, incid_h_t); 
}

model{
    
    // probably need to add a prior on los_mean
    
    target+= neg_binomial_lpmf(cencus_h | census_h_calc, 0.5); // prior on length of stay
}

// generated quantities {
//   // Generate hospital stays for N patients
//   int hospital_stays[N];
//   
//   # call function, generate hosp stays 
//   hospital_stays = covidhosp_stay_funct(N, los);
// 
// }