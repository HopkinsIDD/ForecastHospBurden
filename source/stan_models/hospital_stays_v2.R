# translate stan code into R --------------
nun_matches <- function(x, y, a){
  n <- 0
  for (i in 1:length(x)){
    if (a >= x[i] & a <= y[i]){
      n <- n + 1
    }
  }
  return(n)
}

calc_hosp_end_t <- function(N, los_mean, incid_h_t){
  end_hosp_t <- rep(0, N)
  for (n in 1:N){
    los_calc <- rnegbin(1, mu = los_mean, theta = 0.5)
    end_hosp_t[n] <- los_calc + incid_h_t[n] - 1
  }
  return(end_hosp_t)
}

covidhosp_census_funct <- function(N, T2, los_mean, incid_h_t){
  end_hosp_t <- calc_hosp_end_t(N, los_mean, incid_h_t)
  I <- max(end_hosp_t) # add empriical max to this 
  #   // create I integer that is the max of T2 and the empirical max of end_hosp_t [FIX THIS]
  
  census_h_calc <- rep(0, I)
  for (i in 1:I){
    census_h_calc[i] <- num_matches(incid_h_t, end_hosp_t, i)
  }
  return(census_h_calc)
}













