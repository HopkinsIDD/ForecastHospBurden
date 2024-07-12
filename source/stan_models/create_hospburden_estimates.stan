functions {
    // write r functions in stan 
    
    // generate hospital stays for n patients
    int[] covidhosp_stay_funct(int n, real los) { 
      int result[n];
      for (i in 1:n) {
        result[i] = neg_binomial_2_rng(los, 0.5);
      }
      return result;
    }
    
}
data {
    int<lower=0> N;                // Number of obs
    vector[N] observed_incidH; // observed incident hospitalizations
    vector[N] observed_total_hosp; // observed total hosp
}
// option 1: estimate LOS given observed incident and total hospitalizaiton
// option 2: estimate LOS and estimated total hospitalizations given observed incident and total hospitalizations
parameters {
    real<lower=0> los;  // Length of stay parameter to optimize
}
transformed parameters {
    vector[N] expected_total_hosp; // expected total hosp (model prediction)
    expected_total_hosp = observed_incidH * los; // idea for now, need to update so this is correc,
}
model {
    los ~ uniform(3, 15); // what to do if don't want to bound the parameter?

    // Calculate squared difference between observed and expected total hospitalizations
    vector[N] sq_difference;
    sq_difference = pow(observed_total_hosp - expected_total_hosp, 2);

    // Sum of squared differences as the optimization criterion
    target += sum(sq_difference);
}

// create_hosp_dates

model {
    los ~ uniform(3, 15); // what to do if don't want to bound the parameter?
    // expected_total_hosp drawn from some distribution of los in the model
    expected_total_hosp ~ rnbinom(incident_total_hosp, los, 0.5);
}


