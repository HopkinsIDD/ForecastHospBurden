# Generate Poisson distributed integers ----------------------------------------
lambda <- 5 # Mean parameter
n <- 1000       # Number of samples
data_poisson <- rpois(n, lambda)

# Plot histogram
hist(data_poisson, breaks = 15, main = "Poisson Distribution", xlab = "Values")

# Generate Negative Binomial distributed integers ----------------------------------------
meanLOS <- 10   # Mean length of stay (days)

# Assuming size parameter to be calculated based on empirical data or assumption
size <- 100     # approzimates the mean when prob = 0.5 
#prob <- meanLOS / (size + meanLOS)  # Calculate probability of discharge per day
prob <- 0.5 #c(.3, .5, .7) 
n <- 1000      # Number of patients or observations

data_nbinom <- rnbinom(n, size, prob)
mean(data_nbinom)
# Plot histogram
hist(data_nbinom, breaks = 15, main = "Negative Binomial Distribution", xlab = "Values")

# Generate Gamma distributed integers ----------------------------------------

shape <-10 # Shape parameter (integer > 0) #this centers mean at 5 days with rate of 1, so shape = mean when rate = 1
rate <- 1    # Rate parameter (mean = shape / rate)
n <- 1000     # Number of samples
data_gamma <- rgamma(n, shape, rate)

# Plot histogram
hist(data_gamma, breaks = 15, main = "Gamma Distribution (integer parameterization)", xlab = "Values")
mean(data_gamma)
# Generate Logarithmic Series distributed integers ----------------------------------------
# Desired mean of the variable
mean_var <- 5

meanlog <- log(mean_var) - 0.5 * sdlog^2
sdlog <- 1
n <- 1000     # Number of samples
data_logseries <- rlnorm(n, meanlog, sdlog)

# Plot histogram
hist(data_logseries, breaks = 15, main = "Logarithmic Series Distribution", xlab = "Values")
mean(data_logseries)

