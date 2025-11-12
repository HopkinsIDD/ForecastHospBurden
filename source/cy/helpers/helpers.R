# --- predictH ---
#' @title Predict Active Hospitalisations
#' @param mu = mean duration of hospital stay
#' @param k = dispersion parameter of the negative binomial distribution
#' @param max_stay = maximum duration of hospital stay to consider
#' @param admissions = vector of daily new hospital admissions
#' @return predicted active hospitalisations
#'
#' @details
#' Predicts the number of active hospitalisations based on mu, k, and daily admissions.

predictH <- function(mu, k, max_stay = 50, admissions) {
  survival_probs <- 1 - pnbinom(0:max_stay, size = k, mu = mu)
  survival_probs[!is.finite(survival_probs)] <- 0

  predicted <- convolve(admissions, rev(survival_probs), type = "open")
  predicted <- predicted[1:length(admissions)]
  predicted[!is.finite(predicted)] <- 0
  return(predicted)
}

# --- negbin_sse ---
#' @title Negative Binomial SSE
#' @param params = vector of log(mu) and log(k)
#' @param admissions = vector of daily new hospital admissions
#' @param active_hosp = vector of observed active hospitalisations
#' @return sum of squared errors between observed and predicted active hospitalisations
#'
#' @details
#' Computes the sum of squared errors between observed and predicted active hospitalisations
negbin_sse <- function(params, admissions, active_hosp) {
  mu <- exp(params[1])
  k <- exp(params[2])
  predicted <- predictH(mu = mu, k = k, admissions = admissions)
  sse <- sum((active_hosp - predicted)^2)

  if (!is.finite(sse)) {
    return(1e10)
  } # large penalty instead of NA/Inf
  return(sse)
}

# --- bootstrap_nonparametric ---
#' @title Non-parametric Bootstrap for Parameter Uncertainty
#' @param data = data frame with `admissions` and `active_hosp`
#' @param mu_hat = fitted mean duration of hospital stay
#' @param k_hat = fitted dispersion parameter
#' @param n_boot = number of bootstrap samples
#' @return matrix of bootstrap parameter estimates (mu, k)
#' @details
#' Performs a non-parametric bootstrap by resampling the residuals from the fitted model.
#' For each bootstrap sample, it refits the model to obtain new parameter estimates.
#' This allows us to estimate the uncertainty in the parameter estimates.
#' The function returns a matrix where each row corresponds to a bootstrap sample
#' and the columns correspond to the parameters (mu, k).
bootstrap_nonparametric <- function(data, mu_hat, k_hat, n_boot = 1000) {
  admissions <- data$admissions
  active_hosp <- data$active_hosp
  n_obs <- length(admissions)

  # Residuals = Observed - Predicted
  predicted <- predictH(mu = mu_hat, k = k_hat, admissions = admissions)
  residuals <- active_hosp - predicted

  boot_params <- matrix(NA, nrow = n_boot, ncol = 2)
  for (i in 1:n_boot) {
    # Resample residuals with replacement
    boot_residuals <- sample(residuals, size = n_obs, replace = TRUE)
    boot_active_hosp <- predicted + boot_residuals
    boot_active_hosp <- pmax(boot_active_hosp, 0) # Ensure no negative hospitalisations

    # Refit model to bootstrap sample
    boot_fit <- optim(
      par = c(log(mu_hat), log(k_hat)),
      fn = negbin_sse,
      admissions = admissions,
      active_hosp = boot_active_hosp,
      lower = c(log(1e-3), log(1e-3)),
      method = "L-BFGS-B"
    )
    boot_params[i, ] <- exp(boot_fit$par)
  }
  return(boot_params)
}
