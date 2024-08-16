functions {
    int num_matches(vector x, real[] y, int a){
        int n = 0;
        for (i in 1:size(x)){
            if (a >= x[i] && a <= y[i]){
                n += 1;
            }
        }
        return n;
    }

    int get_interval(int t, int interval_size) {
        return 1 + (t - 1) / interval_size;
    }
}

data {
    int<lower=0> T;                
    int<lower=0> N;                
    vector<lower=0>[N] incid_h_t;
    vector<lower=0>[T] census_h; 
    real<lower=0> los_prior; 
    int<lower=1> interval_size;  // The size of each time interval (e.g., 90 days)
}

parameters {
    int<lower=1> num_intervals = ceil(T / interval_size);
    vector<lower=0>[num_intervals] los_mean;  
}

transformed parameters {
    vector<lower=0>[num_intervals] neg_binom_alpha;
    for (j in 1:num_intervals) {
        neg_binom_alpha[j] = los_mean[j] * 0.5;
    }
}

model {
    los_mean ~ normal(los_prior, 2);
}

generated quantities {
    int<lower=0> los_indiv[N];
    array[N] real<lower=0> end_hosp_t;
    array[T] real<lower=0> census_h_calc;

    for (i in 1:N) {
        int interval = get_interval(incid_h_t[i], interval_size);
        los_indiv[i] = neg_binomial_rng(neg_binom_alpha[interval], 0.5);
        end_hosp_t[i] = incid_h_t[i] + los_indiv[i] - 1;
    }
    
    for (i in 1:T) {
        census_h_calc[i] = num_matches(incid_h_t, end_hosp_t, i);
    }
}
