# ==============================================================================
# 03_bootstrap_reserving.R
#
# Over-Dispersed Poisson (ODP) bootstrap on chain-ladder development residuals,
# following the England & Verrall (2002) approach widely used in practice
# (e.g. underlies the `BootChainLadder` function in the CAS-endorsed
# `ChainLadder` R package). Produces a full predictive distribution of
# reserves rather than a single point estimate, so we can report percentiles,
# a 75%/90% reserve range, and a coefficient of variation by accident year
# and in total.
#
# Mechanics:
#   1. Fit chain-ladder to get fitted (expected) cumulative values and the
#      implied incremental triangle.
#   2. Compute Pearson residuals of incremental losses vs fitted incrementals.
#   3. Resample residuals with replacement, rebuild a pseudo-triangle, re-fit
#      chain-ladder development factors on each pseudo-triangle, and project
#      ultimates.
#   4. Add process variance (an ODP sampling step) on top of each pseudo-
#      ultimate to capture process risk, not just parameter risk.
#   5. Repeat n_boot times to build the full reserve distribution.
# ==============================================================================

#' Convert a cumulative triangle into an incremental triangle
#' @param triangle n x n cumulative triangle (NA in unobserved cells)
to_incremental <- function(triangle) {
  n <- nrow(triangle)
  inc <- triangle
  inc[, 2:n] <- triangle[, 2:n] - triangle[, 1:(n - 1)]
  inc
}

#' Convert an incremental triangle back into cumulative form
to_cumulative <- function(incremental) {
  n <- nrow(incremental)
  cum <- incremental
  for (k in 2:n) cum[, k] <- cum[, k - 1] + incremental[, k]
  cum
}

#' Fitted incremental triangle implied by chain-ladder development factors,
#' back-calculated from each accident year's fitted ultimate.
#' @param triangle observed cumulative triangle
#' @param cl       output of chain_ladder()
fitted_incremental_triangle <- function(triangle, cl) {
  n <- nrow(triangle)
  cdf <- cl$cdf
  ultimate <- cl$ultimate

  fitted_cum <- matrix(NA_real_, n, n, dimnames = dimnames(triangle))
  for (i in 1:n) {
    for (k in 1:n) {
      fitted_cum[i, k] <- ultimate[i] / cdf[k]
    }
  }
  to_incremental(fitted_cum)
}

#' Run an ODP bootstrap on a cumulative loss triangle.
#'
#' @param triangle    n x n cumulative loss triangle
#' @param n_boot      number of bootstrap iterations (default 1000)
#' @param tail_factor passed to chain_ladder()
#' @param seed        RNG seed for reproducibility
#' @return list with:
#'   sims            n_boot x n matrix of simulated ultimate losses by AY
#'   sims_reserve    n_boot x n matrix of simulated reserves by AY
#'   total_reserve_sims  vector of length n_boot, total reserve per sim
#'   point_estimate  chain-ladder point reserve (for comparison)
#'   summary         data.frame with mean, sd, CV, and percentiles by AY + total
bootstrap_reserve <- function(triangle, n_boot = 1000, tail_factor = 1.0, seed = 42) {
  set.seed(seed)
  n <- nrow(triangle)
  ay <- rownames(triangle)

  cl <- chain_ladder(triangle, tail_factor = tail_factor)
  inc_obs <- to_incremental(triangle)
  inc_fit <- fitted_incremental_triangle(triangle, cl)

  # Pearson scale parameter (phi) for the ODP process-variance step:
  # phi = sum(residuals^2) / (N - p), where N = # observed incremental cells,
  # p = # of parameters estimated (n development factors + n AY levels - 1,
  # using the standard chain-ladder parameter count).
  obs_mask <- !is.na(inc_obs)
  # Guard against near-zero fitted increments at the tail (e.g. the last
  # development column when ldf -> 1.0, where the fitted incremental can be
  # ~0 and produce an exploding/undefined Pearson residual). Cells with a
  # fitted increment below this floor are excluded from the residual pool.
  fit_floor <- 1e-6 * max(abs(inc_fit[obs_mask]), na.rm = TRUE)
  usable <- obs_mask & abs(inc_fit) > fit_floor

  resid_pearson <- matrix(NA_real_, n, n, dimnames = dimnames(triangle))
  resid_pearson[usable] <- (inc_obs[usable] - inc_fit[usable]) / sqrt(abs(inc_fit[usable]))

  N_obs <- sum(usable)
  p_params <- (n - 1) + n  # (n-1) age-to-age factors + n accident-year levels
  dof <- max(N_obs - p_params, 1)
  phi <- sum(resid_pearson[usable]^2) / dof

  resid_pool <- resid_pearson[usable]
  # Standardize residuals (degrees-of-freedom adjustment), standard step in
  # England & Verrall bootstrap to avoid downward-biased resampled variance
  resid_pool <- resid_pool * sqrt(N_obs / dof)

  sims_ultimate <- matrix(NA_real_, n_boot, n, dimnames = list(NULL, ay))
  sims_reserve  <- matrix(NA_real_, n_boot, n, dimnames = list(NULL, ay))

  latest_dev_age <- cl$latest_dev_age
  latest <- cl$latest

  for (b in 1:n_boot) {
    # --- Step 1: build a pseudo cumulative triangle by resampling residuals.
    # Cells excluded from `usable` (near-zero fitted increments) keep their
    # fitted value exactly -- they carry no meaningful residual information.
    resampled_resid <- matrix(0, n, n, dimnames = dimnames(triangle))
    resampled_resid[usable] <- sample(resid_pool, N_obs, replace = TRUE)

    pseudo_inc <- inc_fit
    pseudo_inc[obs_mask] <- inc_fit[obs_mask] + resampled_resid[obs_mask] * sqrt(abs(inc_fit[obs_mask]))
    # Guard against pathological negative incrementals flipping monotonicity
    pseudo_inc[obs_mask] <- pmax(pseudo_inc[obs_mask], -inc_fit[obs_mask] * 0.95)

    pseudo_cum <- to_cumulative(pseudo_inc)
    pseudo_cum[!obs_mask] <- NA

    # --- Step 2: re-fit chain-ladder on the pseudo triangle (parameter risk)
    cl_b <- tryCatch(chain_ladder(pseudo_cum, tail_factor = tail_factor),
                      error = function(e) NULL)
    if (is.null(cl_b) || any(!is.finite(cl_b$ultimate))) {
      sims_ultimate[b, ] <- NA
      next
    }

    # Future incremental losses implied by the re-fit model, by AY, summed
    # across the unobserved development ages
    cdf_b <- cl_b$cdf
    fut_mean_ultimate <- latest * (cdf_b[latest_dev_age])
    future_mean_reserve <- pmax(fut_mean_ultimate - latest, 0)

    # --- Step 3: process variance -- simulate actual future payments as
    # Gamma(shape = mean^2/(phi*mean), scale = phi) i.e. Var = phi * mean,
    # the standard ODP process-error step (Gamma approximates the ODP here
    # since it's defined for continuous severities and matches the
    # mean-variance relationship of an over-dispersed Poisson)
    sim_reserve <- numeric(n)
    for (i in 1:n) {
      mu <- future_mean_reserve[i]
      if (mu <= 0) {
        sim_reserve[i] <- 0
      } else {
        shape <- mu / phi
        sim_reserve[i] <- rgamma(1, shape = shape, scale = phi)
      }
    }

    sims_reserve[b, ] <- sim_reserve
    sims_ultimate[b, ] <- latest + sim_reserve
  }

  total_reserve_sims <- rowSums(sims_reserve, na.rm = FALSE)
  valid <- !is.na(total_reserve_sims)
  if (mean(valid) < 0.9) {
    warning(sprintf("%.0f%% of bootstrap iterations failed to converge; results may be unstable.",
                     100 * (1 - mean(valid))))
  }

  summary_by_ay <- data.frame(
    AccidentYear = ay,
    PointReserve = round(cl$reserve, 0),
    BootMean     = round(colMeans(sims_reserve, na.rm = TRUE), 0),
    BootSD       = round(apply(sims_reserve, 2, sd, na.rm = TRUE), 0),
    CV           = round(apply(sims_reserve, 2, sd, na.rm = TRUE) /
                            pmax(colMeans(sims_reserve, na.rm = TRUE), 1), 4),
    P10          = round(apply(sims_reserve, 2, quantile, 0.10, na.rm = TRUE), 0),
    P50          = round(apply(sims_reserve, 2, quantile, 0.50, na.rm = TRUE), 0),
    P75          = round(apply(sims_reserve, 2, quantile, 0.75, na.rm = TRUE), 0),
    P90          = round(apply(sims_reserve, 2, quantile, 0.90, na.rm = TRUE), 0),
    P99          = round(apply(sims_reserve, 2, quantile, 0.99, na.rm = TRUE), 0)
  )

  total_row <- data.frame(
    AccidentYear = "TOTAL",
    PointReserve = round(cl$total_reserve, 0),
    BootMean     = round(mean(total_reserve_sims, na.rm = TRUE), 0),
    BootSD       = round(sd(total_reserve_sims, na.rm = TRUE), 0),
    CV           = round(sd(total_reserve_sims, na.rm = TRUE) /
                            mean(total_reserve_sims, na.rm = TRUE), 4),
    P10 = round(quantile(total_reserve_sims, 0.10, na.rm = TRUE), 0),
    P50 = round(quantile(total_reserve_sims, 0.50, na.rm = TRUE), 0),
    P75 = round(quantile(total_reserve_sims, 0.75, na.rm = TRUE), 0),
    P90 = round(quantile(total_reserve_sims, 0.90, na.rm = TRUE), 0),
    P99 = round(quantile(total_reserve_sims, 0.99, na.rm = TRUE), 0)
  )
  rownames(summary_by_ay) <- NULL
  rownames(total_row) <- NULL

  list(
    method              = "ODP Bootstrap",
    sims_ultimate       = sims_ultimate,
    sims_reserve        = sims_reserve,
    total_reserve_sims  = total_reserve_sims,
    point_estimate      = cl,
    phi                 = phi,
    summary             = rbind(summary_by_ay, total_row)
  )
}
