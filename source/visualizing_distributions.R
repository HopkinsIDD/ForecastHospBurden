# Generate Poisson distributed integers ----------------------------------------
lambda <- 2.5  # Mean parameter
n <- 1000       # Number of samples
data_poisson <- rpois(n, lambda)

# Plot histogram
hist(data_poisson, breaks = 15, main = "Poisson Distribution", xlab = "Values")

# Generate Negative Binomial distributed integers ----------------------------------------
size <- 5    # Number of failures parameter
prob <- 0.3  # Success probability parameter
n <- 1000     # Number of samples
data_nbinom <- rnbinom(n, size, prob)

# Plot histogram
hist(data_nbinom, breaks = 15, main = "Negative Binomial Distribution", xlab = "Values")

# Generate Gamma distributed integers ----------------------------------------
shape <- 2   # Shape parameter (integer > 0)
rate <- 1    # Rate parameter
n <- 1000     # Number of samples
data_gamma <- rgamma(n, shape, rate)

# Plot histogram
hist(data_gamma, breaks = 15, main = "Gamma Distribution (integer parameterization)", xlab = "Values")

# Generate Logarithmic Series distributed integers ----------------------------------------
prob <- 0.4  # Success probability parameter
n <- 1000     # Number of samples
data_logseries <- rgeom(n, prob)

# Plot histogram
hist(data_logseries, breaks = 15, main = "Logarithmic Series Distribution", xlab = "Values")
