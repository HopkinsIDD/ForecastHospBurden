# =============================================
#   LOS: predict, fit, bootstrap
# =============================================
# Core identity:
#   census(t) = sum over s <= t of admissions(s) * P(LOS > t - s)
#
# We fit the survival curve P(LOS > d) so that its convolution with
# observed admissions matches observed census. A residual bootstrap
# gives parameter uncertainty, used later when the admission input is
# deterministic (the "truth+LOS" path).

# Predicted census from admissions and a survival curve.
# Non-finite outputs (from bad parameters during optim) are set to 0.
predict_census <- function(survival, admissions) {
  pred <- convolve(admissions, rev(survival), type = "open")[seq_along(
    admissions
  )]
  pred[!is.finite(pred)] <- 0
  pred
}

# Sum of squared errors between observed and predicted census, ignoring
# the first `skip` days (their census depends on admissions from before
# the series starts). Returns a large number if the prediction blows up
# so optim keeps moving instead of getting stuck.
los_sse <- function(log_params, admissions, census, surv_fn, skip) {
  survival <- surv_fn(log_params)
  survival[!is.finite(survival)] <- 0
  pred <- predict_census(survival, admissions)
  idx <- seq(skip + 1, length(admissions))
  sse <- sum((census[idx] - pred[idx])^2)
  if (!is.finite(sse)) 1e10 else sse
}

# Residual bootstrap for parameter uncertainty.
#   1. Predict census at the point estimate.
#   2. Compute residuals on the fit window.
#   3. Build pseudo-censuses = predicted + resampled residuals (>= 0).
#   4. Refit on each.
# Returns an n_boot x n_params matrix of parameter draws.
bootstrap_residual <- function(data, params_hat, los_dist, skip, n_boot = 100) {
  log_params <- log(params_hat)
  survival <- los_dist$surv_fn(log_params)
  survival[!is.finite(survival)] <- 0
  predicted <- predict_census(survival, data$admissions)

  idx <- seq(skip + 1, nrow(data))
  resid <- data$census[idx] - predicted[idx]

  boot_params <- matrix(NA_real_, nrow = n_boot, ncol = length(params_hat))
  colnames(boot_params) <- los_dist$param_names

  for (i in seq_len(n_boot)) {
    boot_census <- data$census
    boot_census[idx] <- pmax(
      predicted[idx] + sample(resid, length(idx), TRUE),
      0
    )
    fit <- optim(
      par = log_params,
      fn = los_sse,
      admissions = data$admissions,
      census = boot_census,
      surv_fn = los_dist$surv_fn,
      skip = skip,
      lower = los_dist$lower,
      method = "L-BFGS-B"
    )
    boot_params[i, ] <- exp(fit$par)
  }
  boot_params
}

# Fit LOS for one state-season.
#   skip    days at the start to ignore in the loss (default MAX_STAY).
#   n_boot  residual bootstrap iterations.
#
# Returns:
#   params          point estimate on the natural scale
#   survival        P(LOS > d) vector at the point estimate
#   boot_params     n_boot x n_params matrix of bootstrap parameter
#                   draws. Kept for parameter-space diagnostics
#                   (e.g. misc/ridge_plots.R).
#   boot_survivals  (MAX_STAY+1) x n_boot matrix of bootstrap survival
#                   vectors. Computed here so forecast time only needs
#                   to read survival curves, never re-evaluate the
#                   distribution kernel.
#   sse             loss at the point estimate
#   n_fit           number of days used in the loss
#   ci              tibble with pointwise 95% prediction bands over the
#                   fit window: (date, observed, lower, median, upper)
fit_los <- function(data, los_dist, skip = MAX_STAY, n_boot = 100) {
  fit <- optim(
    par = los_dist$init,
    fn = los_sse,
    admissions = data$admissions,
    census = data$census,
    surv_fn = los_dist$surv_fn,
    skip = skip,
    lower = los_dist$lower,
    method = "L-BFGS-B"
  )
  params <- setNames(exp(fit$par), los_dist$param_names)
  survival <- los_dist$surv_fn(fit$par)
  survival[!is.finite(survival)] <- 0

  # Bootstrap parameter draws -> survival vectors, once.
  boot <- bootstrap_residual(data, params, los_dist, skip, n_boot)
  boot_survivals <- apply(log(boot), 1, los_dist$surv_fn)
  boot_survivals[!is.finite(boot_survivals)] <- 0

  idx <- seq(skip + 1, nrow(data))
  preds <- apply(boot_survivals, 2, \(s) predict_census(s, data$admissions)[idx])

  list(
    params = as.list(params),
    survival = survival,
    boot_params = boot,
    boot_survivals = boot_survivals,
    sse = fit$value,
    n_fit = length(idx),
    ci = tibble(
      date = data$date[idx],
      observed = data$census[idx],
      lower = apply(preds, 1, quantile, 0.025),
      median = apply(preds, 1, median),
      upper = apply(preds, 1, quantile, 0.975)
    )
  )
}
