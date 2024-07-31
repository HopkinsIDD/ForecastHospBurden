functions {
    // DONT USE THIS VERSION
    
    // generate hospital stays for N patients
    int[] covidhosp_stayend_funct(int N, real los_mean, int[] incid_h_t) { 
        // integer N, real value los_mean, integer with time of incidH
        int los_calc[N]; // array to store LOS for N patients 
        int census_h_calc[N]; // array to store end of hospital stay times
        
        // Generate length of stay ests using negative binomial distribution
        los_calc = neg_binomial_rng(los_mean, 0.5); // Uses a mean length of stay and dispersion parameter
        // rng random number generator 
        
        // Calculate the end of hospitalization time for N Patients
        census_h_calc = los_calc + incid_h_t - 1; // day of adnmissions, 
        // mean LOS + day of hospitalization - 1 = day of end hospitalization
        
        // returns last day of hospital admissiom, for each adnmission now discharge date
        return census_h_calc; // Return the calculated end times
    }
    // genereate LOS for each individual
    
    // Function to count the number of times a value a falls within ranges defined by x and y
    int num_matches(int[] x, int[] y, int a) {
        int n = 0; // initalize counter n = 0 
        for (i in 1:size(x)){
            // Check if 'a' falls between x[i] and y[i] 
            if (a >= x[i] & a <= y[i]){ // does a fall within range of x and y
            // how many idv fall in this range 
              n += 1; // Increment count if a match is found
            }
        }
        return n; // Return the total count of matches
    }
    // get into matching 
    // not built into R 
    // ID matches with different numbers 
    // two vectors x, y -> our end and start hospitalization (admission and discharge)
    // a is the ... 
    // for each value of i does it fall within the rang eof x to y
    // each indv has that range 
    // how many idv fall within that range
    
    // Function to calculate the end of hospitalization times for N patients
    int[] calc_hosp_end_t(int N, real los_mean, int[] incid_h_t) { // num idv, LOS mean, vector of admission days (not dates)
        
        array[N] int<lower=0> end_hosp_t; // Array to store the end times of hospitalization
        for (n in 1:N) {
            int los_calc; // Variable to store length of stay for each patient
            los_calc = neg_binomial_rng(los_mean, 0.5); // Generate length of stay for patient n
            end_hosp_t[n] = los_calc + incid_h_t[n] - 1; // Calculate end time of hospitalization
        }
        // loop through all idv
        // for each indv, do random number generator 
        return end_hosp_t; // Return the array of end times
    }
    
    // Function to calculate the hospital census for each time point T
    int[] covidhosp_census_funct(int N, int T, real los_mean, int[] incid_h_t) { 
        // num idv, num time points, LOS mean, vector of admission days
        
        // start with hosp end date bc running function with
        array[N] int<lower=0> end_hosp_t; // Array to store end times of hospitalization
        end_hosp_t = calc_hosp_end_t(N, los_mean, incid_h_t); // Get end times
        // just calling function to create vector 
        
        int<lower=0> I; // Maximum end time across all patients
        I = max(end_hosp_t); // ;last date that smoeone could have been discharged on, allows to cycle through all the dates, go through and accumalte all hosp admission days
        
        array[I]<lower=0> int census_h_calc; // Array to store hospital census counts
        for (i in 1:I) {
            census_h_calc[i] = num_matches(incid_h_t, end_hosp_t, i); // Count the number of patients in the hospital at time i
        // num_matches gnerates single number every time, ie 6 hosp on july 15 
        }
        // do a num match
        // cycle through every day 
        // for every day, how many ppl were currently hospitalizated 
        // a is the census date 
        // ie july 15, every single admission range 
        // were hosp before july 15 and still hospitalized on july 15 
        return census_h_calc; // Return the array of census counts
    }
    // adds up the census days, need to do accumulation
    
}


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
        I = max(end_h_t); // going to max of generated date or empirical date
        array[I]<lower=0> int census_h_calc;
        for (i in 1:I) {
            census_h_calc[i] = num_matches(incid_h_t, end_h_t, i)
        }
        return census_h_calc;
    }
}
// say july 15 is last hospitalization date in census date, 
// rng may produce 1 hosp on july 16, make sure that
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
    // may need to be in a transformed data piece 
    // R is interpeted by computer line by line, stand is interpreted all at once
    // so everything is all at once 
    // rewerite logic in R to make sure things are doing what they should be doig
    
    // calling functions been writing, based on # ppl los mean

}
// goal is to identify most appropatiate LOS estimate for however we define it 
// stan works by stating
// 
transformed data {
    // create a new census_h that is same lenghts as the calculated census_h_calc
    // fill in the values of census_h that are less than the length of census_h_calc
    
    // code isn't quite, but getting there
    // just need to fix T 
    
}
model{
    
    // probably need to add a prior on los_mean
    los_mean ~ normal(5, 2); // prior on length of stay
    // give los prior, this will draw LOS from this distribution, which feeds into above etc
    // target will work to minimize the error function
    // vs calc likelihood and minzimin glof likelihood
    target+= neg_binomial_lpmf(cencus_h | census_h_calc, 0.5); // prior on length of stay
    //census_empirical ~ some_distribution(census_parameter, sigma)
    // we know this, so give starting value estimate parameter that most approporaitely  defines distribution 
    // bc using LOS_mean to generate census h calc, this doesnt quite work
}
// census_h_calc - is for each time stpe

// hw 
// make sure that lengths are the same 
// so everything is the same lenght
// neg_binomial_lpmf
//     los_mean ~ normal(5, 2); is normally distributed
// add in period to give values 
// trying to minizimise error between     target+= neg_binomial_lpmf(cencus_h | census_h_calc, 0.5); // prior on length of stay
// Q what do we want for distribution.. already using neg binomial, so what do we want to use
// on a single day .. prob fine to leave neg binom
// once fix book keeping think model should run

// generated quantities {
//   // Generate hospital stays for N patients
//   int hospital_stays[N];
//   
//   # call function, generate hosp stays 
//   hospital_stays = covidhosp_stay_funct(N, los);
// 
// }