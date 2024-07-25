functions {

    // Function to calculate the end of hospital stays for N patients
    int[] covidhosp_stayend_funct(int N, real los_mean, int[] incid_h_t) { 
        int los_calc[N]; // Array to store length of stay calculations
        int census_h_calc[N]; // Array to store end of hospital stay times
        
        // Generate length of stay using negative binomial distribution
        los_calc = neg_binomial_rng(los_mean, 0.5); // Uses a mean length of stay and a dispersion parameter
        
        // Calculate the end of hospitalization time by adding the length of stay to the incident day
        census_h_calc = los_calc + incid_h_t - 1; 
        
        return census_h_calc; // Return the calculated end times
    }
    
    // Function to count the number of times a value a falls within ranges defined by x and y
    int num_matches(int[] x, int[] y, int a) {
        int n = 0; // Counter for number of matches
        for (i in 1:size(x)){
            // Check if 'a' falls between x[i] and y[i] (inclusive)
            if (a >= x[i] & a <= y[i]){
              n += 1; // Increment count if a match is found
            }
        }
        return n; // Return the total count of matches
    }
    
    // Function to calculate the end of hospitalization times for N patients
    int[] calc_hosp_end_t(int N, real los_mean, int[] incid_h_t) { 
        
        array[N] int<lower=0> end_hosp_t; // Array to store the end times of hospitalization
        for (n in 1:N) {
            int los_calc; // Variable to store length of stay for each patient
            los_calc = neg_binomial_rng(los_mean, 0.5); // Generate length of stay for patient n
            end_hosp_t[n] = los_calc + incid_h_t[n] - 1; // Calculate end time of hospitalization
        }
        return end_hosp_t; // Return the array of end times
    }
    
    // Function to calculate the hospital census for each time point T
    int[] covidhosp_census_funct(int N, int T, real los_mean, int[] incid_h_t) { 

        array[N] int<lower=0> end_hosp_t; // Array to store end times of hospitalization
        end_h_t = calc_hosp_end_t(N, los_mean, incid_h_t); // Get end times
        
        int<lower=0> I; // Maximum end time across all patients
        I = max(end_h_t);
        
        array[I]<lower=0> int census_h_calc; // Array to store hospital census counts
        for (i in 1:I) {
            census_h_calc[i] = num_matches(incid_h_t, end_h_t, i); // Count the number of patients in the hospital at time i
        }
        return census_h_calc; // Return the array of census counts
    }
}

data {
    int<lower=0> T;                // Number of time points (dates)
    int<lower=0> N;                // Number of hospitalizations (observations)
    array[N] int<lower=0> incid_h_t; // Array of individual hos
