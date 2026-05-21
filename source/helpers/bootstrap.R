# ============================================================
#   Residual bootstrap for the truth+LOS path
# ============================================================
# censcast::fit_los() returns only the point estimate. The
# truth+LOS comparison in this repo needs LOS uncertainty
# intervals, so we re-fit on residual resampled census n_boot
# times and propagate the resulting survival curves.

# Convolve admissions with a survival vector. Same one liner that
# censcast uses internally; kept here so we don't reach into its
# private namespace.
predict_census <- function(los, admissions) {
  pred <- stats::convolve(admissions, rev(los), type = "open")[
    seq_along(admissions)
  ]
  pred[!is.finite(pred)] <- 0
  pred
}

# Residual bootstrap of censcast::fit_los().
#
# data      Data frame with `admissions` and `census` columns
#           (one row per time step), as accepted by fit_los().
# fit       Output of censcast::fit_los(data, ...).
# n_boot    Number of bootstrap refits.
# max_stay  Truncation point.
# skip      Time steps to ignore at the start of the loss.
#
# Returns a (max_stay+1) x n_boot matrix of bootstrap survival
# curves.
bootstrap_los <- function(data, fit, n_boot = 100,
                          max_stay = 50, skip = max_stay) {
  family <- attr(fit, "family")
  surv <- as.numeric(fit)
  pred <- predict_census(surv, data$admissions)
  idx <- seq(skip + 1L, nrow(data))
  resid <- data$census[idx] - pred[idx]

  out <- matrix(NA_real_, nrow = length(surv), ncol = n_boot)
  for (i in seq_len(n_boot)) {
    boot_census <- data$census
    boot_census[idx] <- pmax(
      pred[idx] + sample(resid, length(idx), TRUE), 0
    )
    boot_data <- data
    boot_data$census <- boot_census
    out[, i] <- as.numeric(censcast::fit_los(
      boot_data, family = family, max_stay = max_stay, skip = skip
    ))
  }
  out
}

# truth+LOS census forecast for one (state, reference_date).
#
# Convolves the observed admission series with every bootstrap
# survival draw and takes empirical quantiles across draws.
# Returns a hubverse shape tibble or NULL when the admission
# series is incomplete.
truth_los_one <- function(loc, ref_date, boot_surv, hhs,
                          max_stay, max_horizon, q_levels) {
  series <- dplyr::filter(
    hhs,
    .data$state == loc,
    dplyr::between(
      .data$date,
      ref_date - max_stay + 1L,
      ref_date + max_horizon
    )
  ) |>
    dplyr::arrange(.data$date)
  if (nrow(series) < max_stay + max_horizon) {
    return(NULL)
  }

  preds <- apply(boot_surv, 2, \(s) {
    predict_census(s, series$admissions)[-seq_len(max_stay)]
  })
  target_dates <- series$date[-seq_len(max_stay)]
  q_mat <- apply(preds, 1, stats::quantile,
                 probs = q_levels, na.rm = TRUE)

  tibble::tibble(
    model_id        = "truth_LOS",
    location        = loc,
    reference_date  = ref_date,
    target_end_date = rep(target_dates, each = length(q_levels)),
    horizon         = as.integer(target_end_date - ref_date),
    target          = "day inc covid hosp",
    output_type     = "quantile",
    output_type_id  = as.character(rep(q_levels, times = length(target_dates))),
    value           = as.vector(q_mat)
  ) |>
    dplyr::filter(horizon > 0L, horizon <= max_horizon)
}
