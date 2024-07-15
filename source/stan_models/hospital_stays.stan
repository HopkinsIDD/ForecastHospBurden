functions {
    // write r functions in stan 
    
    // generate hospital stays for n patients
    int[] covidhosp_stay_funct(int n, real los) { 
      int result[n];
      for (i in 1:n) {
        result[i] = neg_binomial_rng(los, 0.5);
      }
      return result;
    }
    
}
data {
    int<lower=0> N;                // Number of obs
    //vector[N] observed_incidH; // observed incident hospitalizations
    //vector[N] observed_total_hosp; // observed total hosp
}

parameters {
    real<lower=0> los;  // Length of stay parameter to optimize
}
model{
    
}

//extra output data, don't use until mode working well 
//generated quantities {
  // Generate hospital stays for N patients
  //int hospital_stays[N];
  
  # call function, generate hosp stays 
  //hospital_stays = covidhosp_stay_funct(N, los);

//}