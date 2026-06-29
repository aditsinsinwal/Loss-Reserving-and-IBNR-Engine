# ==============================================================================
# 00_simulate_schedule_p.R
#
# Generates realistic, CAS Schedule P-style cumulative loss triangles for
# multiple lines of business. The CAS Loss Reserve Database (from NAIC
# Schedule P filings) is the de facto industry benchmark dataset for testing
# reserving methods, but it isn't reachable from this environment, so this
# script SIMULATES triangles with the same structure, accident-year/
# development-year layout, and realistic loss-emergence patterns.
#
# To use REAL CAS Schedule P data instead:
#   1. Download from https://www.casact.org/research/index.cfm?fa=loss_reserves_data
#      (e.g. "comauto_pos.csv", "wkcomp_pos.csv", "prodliab_pos.csv", etc.)
#   2. Replace the call to simulate_triangle() in main.R with a loader that
#      pivots the CAS long-format CSV (AccidentYear x DevelopmentYear ->
#      CumulativePaidLoss) into the same matrix shape produced here.
#   3. Everything downstream (chain-ladder, BF, bootstrap) is agnostic to
#      where the triangle came from -- it only needs an n x n matrix of
#      cumulative losses with NA in the lower-right (future) cells.
# ==============================================================================

#' Simulate a cumulative loss triangle for one line of business
#'
#' @param n_years number of accident years (= number of development periods)
#' @param base_premium  earned premium for the oldest accident year
#' @param premium_trend annual growth rate in earned premium
#' @param loss_ratio_mean mean ultimate loss ratio (ELR) for the line
#' @param loss_ratio_sd   accident-year volatility in the ultimate loss ratio
#' @param tail_speed   controls how fast losses develop (higher = faster payout)
#' @param seed         RNG seed for reproducibility
#' @return list(triangle = n x n matrix of cumulative paid losses with NAs in
#'         the unobserved lower triangle, premium = vector of earned premium
#'         by accident year, line = label)
simulate_triangle <- function(n_years = 10,
                               base_premium = 50000000,
                               premium_trend = 0.04,
                               loss_ratio_mean = 0.65,
                               loss_ratio_sd = 0.06,
                               tail_speed = 1.0,
                               seed = 1,
                               line = "Line") {
  set.seed(seed)

  ay <- 1:n_years
  premium <- base_premium * (1 + premium_trend)^(ay - 1)

  # True (unobservable) ultimate loss ratio by accident year -- lognormal-ish
  # noise around the mean, so different AYs have genuinely different ultimates
  ult_loss_ratio <- pmax(0.25, loss_ratio_mean + rnorm(n_years, 0, loss_ratio_sd))
  ultimate_losses <- premium * ult_loss_ratio

  # Development pattern: a Weibull-type cumulative emergence curve, common
  # for approximating paid-loss development in CAS Schedule P style data.
  # cum_pct[d] = % of ultimate paid by the end of development period d
  dev_periods <- 1:n_years
  shape <- 1.4 * tail_speed
  scale <- n_years / 2.2 / tail_speed
  cum_pct <- 1 - exp(-(dev_periods / scale)^shape)
  cum_pct <- cum_pct / max(cum_pct)  # normalize so dev period n_years = 100%
  # Force last period close to ~97-99% reported (small tail beyond triangle,
  # realistic for long-tail lines like WC/GL) unless tail_speed is high
  cum_pct <- cum_pct * (0.97 + 0.02 * tail_speed)
  cum_pct[n_years] <- min(cum_pct[n_years], 0.995)

  # Build full (square) triangle of cumulative losses with multiplicative
  # accident-year-level noise on top of the deterministic curve, so triangles
  # don't look artificially smooth (more like real Schedule P data)
  full_triangle <- matrix(NA_real_, nrow = n_years, ncol = n_years)
  rownames(full_triangle) <- paste0("AY", 2016 + ay - 1)
  colnames(full_triangle) <- paste0("dev", dev_periods)

  for (i in ay) {
    noise <- 1 + rnorm(n_years, 0, 0.02)
    noise <- cummax(pmin(noise, 1.06))  # keep cumulative losses monotonic-ish
    full_triangle[i, ] <- ultimate_losses[i] * cum_pct * noise
  }

  # Enforce monotonic non-decreasing cumulative losses across development
  for (i in ay) {
    full_triangle[i, ] <- cummax(full_triangle[i, ])
  }

  # Mask the lower-right (unobserved future) part of the triangle: for
  # accident year i, we only observe development periods 1..(n_years - i + 1)
  obs_triangle <- full_triangle
  for (i in ay) {
    max_dev_observed <- n_years - i + 1
    if (max_dev_observed < n_years) {
      obs_triangle[i, (max_dev_observed + 1):n_years] <- NA
    }
  }

  list(
    triangle      = obs_triangle,
    full_triangle = full_triangle,   # only for back-testing in validation
    premium       = setNames(premium, rownames(full_triangle)),
    true_ultimate = setNames(ultimate_losses, rownames(full_triangle)),
    line          = line
  )
}

#' Build the set of lines of business used in this engine, matching the mix
#' of major lines reported in CAS Schedule P (Parts 1-4): Commercial Auto,
#' Workers' Comp, General Liability (Other Liability), and Product Liability.
build_schedule_p_lines <- function(n_years = 10) {
  list(
    comauto = simulate_triangle(
      n_years = n_years, base_premium = 40000000, premium_trend = 0.03,
      loss_ratio_mean = 0.62, loss_ratio_sd = 0.05, tail_speed = 1.6,
      seed = 101, line = "Commercial Auto"
    ),
    wkcomp = simulate_triangle(
      n_years = n_years, base_premium = 55000000, premium_trend = 0.025,
      loss_ratio_mean = 0.68, loss_ratio_sd = 0.07, tail_speed = 0.75,
      seed = 102, line = "Workers' Compensation"
    ),
    othliab = simulate_triangle(
      n_years = n_years, base_premium = 30000000, premium_trend = 0.05,
      loss_ratio_mean = 0.71, loss_ratio_sd = 0.09, tail_speed = 0.55,
      seed = 103, line = "General Liability"
    ),
    prodliab = simulate_triangle(
      n_years = n_years, base_premium = 12000000, premium_trend = 0.02,
      loss_ratio_mean = 0.74, loss_ratio_sd = 0.11, tail_speed = 0.45,
      seed = 104, line = "Products Liability"
    )
  )
}
