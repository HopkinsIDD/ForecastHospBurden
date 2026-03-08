# =============================================
#   LOS Distribution Definitions
# =============================================
#' Each dist_* is a list with:
#'   - name, param_names, surv_fn, init, lower
#' To add a new distribution, just define another list.

dist_negbin <- list(
  name = "negbin",
  param_names = c("mu", "k"),
  surv_fn = function(log_params, max_stay = 50) {
    1 - pnbinom(0:max_stay, size = exp(log_params[2]), mu = exp(log_params[1]))
  },
  init = c(log(2), log(1)),
  lower = c(log(1e-6), log(1e-6))
)

dist_normal <- list(
  name = "normal",
  param_names = c("mu", "sigma"),
  surv_fn = function(log_params, max_stay = 50) {
    pmax(1 - pnorm(0:max_stay, mean = exp(log_params[1]), sd = exp(log_params[2])), 0)
  },
  init = c(log(5), log(2)),
  lower = c(log(1e-6), log(1e-6))
)

dist_lognormal <- list(
  name = "lognormal",
  param_names = c("meanlog", "sdlog"),
  surv_fn = function(log_params, max_stay = 50) {
    1 - plnorm(0:max_stay, meanlog = log_params[1], sdlog = exp(log_params[2]))
  },
  init = c(1.5, log(0.5)),
  lower = c(-Inf, log(1e-6))
)

dist_geometric <- list(
  name = "geometric",
  param_names = c("mu"),
  surv_fn = function(log_params, max_stay = 50) {
    mu <- exp(log_params[1])
    (mu / (mu + 1))^(seq(0, max_stay) + 1)
  },
  init = c(log(5)),
  lower = c(log(1e-6))
)

# =============================================
#   Core Functions
# =============================================

#' Convolution: census = admissions * P(LOS > t)
predict_census <- function(survival, admissions) {
  pred <- convolve(admissions, rev(survival), type = "open")[seq_along(admissions)]
  pred[!is.finite(pred)] <- 0
  pred
}

#' SSE objective. First `prefix` days excluded (convolution warm-up).
los_sse <- function(log_params, admissions, active_hosp, surv_fn, prefix) {
  survival <- surv_fn(log_params)
  survival[!is.finite(survival)] <- 0
  pred <- predict_census(survival, admissions)
  idx <- seq(prefix + 1, length(admissions))
  sse <- sum((active_hosp[idx] - pred[idx])^2)
  if (!is.finite(sse)) 1e10 else sse
}

#' Residual bootstrap: resample residuals, refit, repeat.
bootstrap_residual <- function(data, params_hat, los_dist, prefix,
                               n_boot = 100) {
  log_params <- log(params_hat)
  survival <- los_dist$surv_fn(log_params)
  survival[!is.finite(survival)] <- 0
  predicted <- predict_census(survival, data$admissions)

  idx <- seq(prefix + 1, nrow(data))
  resid <- data$active_hosp[idx] - predicted[idx]

  boot_params <- matrix(NA_real_, nrow = n_boot, ncol = length(params_hat))
  colnames(boot_params) <- los_dist$param_names

  for (i in seq_len(n_boot)) {
    boot_census <- data$active_hosp
    boot_census[idx] <- pmax(predicted[idx] + sample(resid, length(idx), TRUE), 0)
    fit <- optim(
      par = log_params, fn = los_sse,
      admissions = data$admissions, active_hosp = boot_census,
      surv_fn = los_dist$surv_fn, prefix = prefix,
      lower = los_dist$lower, method = "L-BFGS-B"
    )
    boot_params[i, ] <- exp(fit$par)
  }
  boot_params
}

#' Fit + bootstrap + prediction CI for one state.
fit_state <- function(data, los_dist, prefix, n_boot) {
  # Point estimate
  fit <- optim(
    par = los_dist$init, fn = los_sse,
    admissions = data$admissions, active_hosp = data$active_hosp,
    surv_fn = los_dist$surv_fn, prefix = prefix,
    lower = los_dist$lower, method = "L-BFGS-B"
  )
  params <- setNames(exp(fit$par), los_dist$param_names)

  # Bootstrap
  boot <- bootstrap_residual(data, params, los_dist, prefix, n_boot)

  # Prediction CI
  idx <- seq(prefix + 1, nrow(data))
  preds <- apply(boot, 1, \(p) {
    s <- los_dist$surv_fn(log(p))
    s[!is.finite(s)] <- 0
    predict_census(s, data$admissions)[idx]
  })

  list(
    params = as.list(params),
    boot_params = boot,
    sse = fit$value,
    n_fit = length(idx),
    ci = tibble(
      date = data$date[idx],
      observed = data$active_hosp[idx],
      lower = apply(preds, 1, quantile, 0.025),
      median = apply(preds, 1, median),
      upper = apply(preds, 1, quantile, 0.975)
    )
  )
}

#' Fit a LOS distribution across all states.
#' Returns a single tibble with params, AIC, and prediction CIs.
fit_los <- function(df, los_dist, prefix = 50, n_boot = 100) {
  message(sprintf("Fitting %s model...", los_dist$name))
  df |>
    nest(.by = state) |>
    mutate(res = future_map(data, fit_state, los_dist, prefix, n_boot,
                            .options = furrr_options(seed = TRUE))) |>
    unnest_wider(res) |>
    unnest_wider(params) |>
    mutate(
      n_params = length(los_dist$param_names),
      aic = n_fit * log(sse / n_fit) + 2 * n_params,
      model = los_dist$name
    )
}
