# translate stan code into R --------------

# set up ------- 

T <- c(1:8) # for example 
# int<lower=0> T;                // Number of dates  -- come back to this to make sure it matched the generated dates
# int<lower=0> N;                // Number of obs hospitalizations
# array[N] int<lower=0> incid_h_t; // individual day of incident hospitalization 
# array[T] int<lower=0> census_h; // census hosp 
# int<lower=0> los_prior; 


# functions -----------------------------------
# two vectors x, y -> our end and start hospitalization (admission and discharge)
# a is the the census date
# for every day, how many ppl were currently hospitalizated 
# ie july 15, every single admission range 
# were hosp before july 15 and still hospitalized on july 15 

num_matches <- function(x, y, a){
  n <- 0
  for (i in 1:length(x)){
    # does a (census date) fall within the range of hosp dates for patient i
    if (a >= x[i] & a <= y[i]){
      n <- n + 1
    }
  }
  return(n)
}

x <- c(1, 3, 6, 2, 1, 5, 7, 2, 8, 4)
y <- c(5, 7, 10, 4, 3, 9, 10, 6, 12, 8)
a <- 10
hosp_count_t <- num_matches(x, y, a)
# goes through all hospitalized patients and checks if they were hospitalized on day `a`
# returns the number of patients hospitalized on `a` day 
hosp_count_t


# Function to calculate the end of hospitalization times for N patients

calc_hosp_end_t <- function(N, los_mean, incid_h_t){
  end_hosp_t <- rep(0, N)
  for (n in 1:N){
    los_calc <- rnbinom(n = 1, size = los_mean, prob = 0.5)
    end_hosp_t[n] <- los_calc + incid_h_t[n] - 1
  }
  return(end_hosp_t)
}
# num idv, LOS mean, vector of admission days (not dates)
N <- 10
los_mean <- 5
incid_h_t <- c(1, 3, 6, 2, 1, 5, 7, 2, 8, 4)
end_hosp_dates_t <- calc_hosp_end_t(N, los_mean, incid_h_t)
# for each patient, calculate the end of hospitalization date
end_hosp_dates_t

#covidhosp_census_funct <- function(N, T2, los_mean, incid_h_t){
covidhosp_census_funct <- function(N, los_mean, incid_h_t){
  end_hosp_t <- calc_hosp_end_t(N, los_mean, incid_h_t) # calculate end hospitalization date 
  # I is the last end of hospitalization date (need appropriate length)
  I <- max(c(end_hosp_t, T)) # add empirical max to this 
  I
  #   // create I integer that is the max of T2 and the empirical max of end_hosp_t [FIX THIS]
  
  census_h_calc <- rep(0, I) # create a vector of 0s of length I (stan syntax)
  for (i in 1:I){ # I is the max of end_hosp_t aka last day of hospitalization
    # for each day, calculate the number of patients hospitalized on that day
    census_h_calc[i] <- num_matches(incid_h_t, end_hosp_t, i)
  }
  return(census_h_calc)
}

N <- 10
los_mean <- 5
incid_h_t <- c(1, 3, 6, 2, 1, 5, 7, 2, 8, 4)
census_hospital <- covidhosp_census_funct(N, los_mean, incid_h_t)
# returns the number of patients hospitalized on each day
census_hospital

# transformed parameters   -------------------------

census_h_calc <- covidhosp_census_funct(N, los_mean, incid_h_t)

# transformed data -------------------------------

census_h_new 
## // create a new cencus_h_new from cencus_h that is the same length as the calculated census_h_calc
## // fill in the values of census_h_new that are greater than the length of census_h with 0
## // this is to make sure that the two arrays are the same length

## // fix this to use the right value for T (maybe T2?)

census_h_new <- rep(0, T)
# array[T] int<lower=0> census_h_new;
# for (i in 1:T) {
#   if (i <= N) {
#     census_h_new[i] = census_h[i];
#   } else {
#     census_h_new[i] = 0;
#   }
# }
# }

# model -----------------------------------------
  
# probably need to add a prior on los_mean
#   los_mean ~ normal(los_prior, 2); // prior on length of stay
#   target += neg_binomial_lpmf(cencus_h | census_h_calc, 0.5); // prior on length of stay
#   
#   // should we rewrite this to minimize the log-likelihood?
# }
# 
# // generated quantities {
#   //   // Generate hospital stays for N patients
#   //   int hospital_stays[N];
#   //   
#     //   # call function, generate hosp stays 
#     //   hospital_stays = covidhosp_stay_funct(N, los);
#     // 
#       // }













