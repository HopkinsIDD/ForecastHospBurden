# =============================================
#   LOS distributions: survival kernels
# =============================================

# Longest hospital stay we model, in days (we assume P(LOS > 50) ~ 0).
# Used three ways, all tied to this one number:
#   1. length of each survival vector returned by surv_fn.
#   2. days skipped at the start of a fit series (their census depends
#      on admissions we don't observe).
#   3. days of observed admissions prepended before a forecast, so the
#      first forecast day already has a full history.
MAX_STAY <- 50L

# Each dist_* bundles everything fit_los() needs:
#   name         short label.
#   param_names  natural-scale parameter names.
#   surv_fn      returns P(LOS > k) for k = 0..max_stay, given
#                log-scale parameters.
#   init, lower  log-scale starting values and bounds for optim().
#
# Parameters are optimised in log space so they stay positive;
# surv_fn exponentiates internally. The production pipeline uses
# dist_negbin only; the others exist for misc/compare_negbin_vs_normal.R.

# Negative binomial LOS (mean mu, dispersion k). Main model.
dist_negbin <- list(
  name = "negbin",
  param_names = c("mu", "k"),
  surv_fn = function(log_params, max_stay = MAX_STAY) {
    1 - pnbinom(0:max_stay, size = exp(log_params[2]), mu = exp(log_params[1]))
  },
  init = c(log(2), log(1)),
  lower = c(log(1e-6), log(1e-6))
)

# Normal LOS (mean mu, sd sigma), discretised at integer steps and
# truncated at zero.
dist_normal <- list(
  name = "normal",
  param_names = c("mu", "sigma"),
  surv_fn = function(log_params, max_stay = MAX_STAY) {
    pmax(
      1 - pnorm(0:max_stay, mean = exp(log_params[1]), sd = exp(log_params[2])),
      0
    )
  },
  init = c(log(5), log(2)),
  lower = c(log(1e-6), log(1e-6))
)

# Log-normal LOS (meanlog, sdlog). meanlog is unconstrained; only
# sdlog is optimised in log space.
dist_lognormal <- list(
  name = "lognormal",
  param_names = c("meanlog", "sdlog"),
  surv_fn = function(log_params, max_stay = MAX_STAY) {
    1 - plnorm(0:max_stay, meanlog = log_params[1], sdlog = exp(log_params[2]))
  },
  init = c(1.5, log(0.5)),
  lower = c(-Inf, log(1e-6))
)

# Geometric LOS parameterised by mean mu: P(LOS > k) = (mu/(mu+1))^(k+1).
# Single-parameter baseline, no dispersion.
dist_geometric <- list(
  name = "geometric",
  param_names = c("mu"),
  surv_fn = function(log_params, max_stay = MAX_STAY) {
    mu <- exp(log_params[1])
    (mu / (mu + 1))^(seq(0, max_stay) + 1)
  },
  init = c(log(5)),
  lower = c(log(1e-6))
)
