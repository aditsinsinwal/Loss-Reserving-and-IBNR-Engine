# ==============================================================================
# 02_bornhuetter_ferguson.R
#
# Bornhuetter-Ferguson method: blends an a priori expected loss ratio (ELR)
# with actual emergence, weighting each by how much of the loss is expected
# to have emerged so far. More stable than chain-ladder for immature/volatile
# accident years, since it doesn't extrapolate off a single thin diagonal.
#
#   Ultimate_BF = Latest + (Premium x ELR) x (1 - %Reported)
#   %Reported   = 1 / CDF_at_latest_age
# ==============================================================================

#' Run the Bornhuetter-Ferguson method on a triangle.
#'
#' @param triangle    n x n cumulative loss triangle
#' @param premium     named vector of earned premium by accident year
#' @param elr         a priori expected loss ratio. Either a single number
#'                     (applied to all AYs) or a named vector per AY.
#'                     If NULL, uses the chain-ladder implied ELR for the
#'                     oldest (most mature) accident years as a proxy a priori.
#' @param tail_factor passed through to compute_cdf()
#' @return list, same shape as chain_ladder() output, plus expected_losses
#'         and pct_reported for transparency
bornhuetter_ferguson <- function(triangle, premium, elr = NULL, tail_factor = 1.0) {
  n <- nrow(triangle)
  ay <- rownames(triangle)

  ldf <- compute_ldf(triangle)
  cdf <- compute_cdf(ldf, tail_factor = tail_factor)

  latest <- numeric(n)
  latest_dev_age <- numeric(n)
  for (i in 1:n) {
    obs_cols <- which(!is.na(triangle[i, ]))
    latest_dev_age[i] <- max(obs_cols)
    latest[i] <- triangle[i, latest_dev_age[i]]
  }
  names(latest) <- ay

  cdf_at_latest <- cdf[latest_dev_age]
  pct_reported <- 1 / cdf_at_latest
  names(pct_reported) <- ay

  # Default a priori ELR: use the chain-ladder ultimate loss ratio from the
  # most mature (oldest) accident years, which is a standard, defensible way
  # to set a BF a priori when an external pricing ELR isn't supplied.
  if (is.null(elr)) {
    cl <- chain_ladder(triangle, tail_factor = tail_factor)
    mature <- latest_dev_age >= quantile(latest_dev_age, 0.5)
    implied_elr <- sum(cl$ultimate[mature]) / sum(premium[ay][mature])
    elr <- setNames(rep(implied_elr, n), ay)
  } else if (length(elr) == 1) {
    elr <- setNames(rep(elr, n), ay)
  }

  expected_losses <- premium[ay] * elr[ay]
  reserve <- expected_losses * (1 - pct_reported)
  ultimate <- latest + reserve

  list(
    method          = "Bornhuetter-Ferguson",
    ldf             = ldf,
    cdf             = cdf,
    latest          = latest,
    latest_dev_age  = latest_dev_age,
    pct_reported    = pct_reported,
    elr             = elr,
    expected_losses = expected_losses,
    ultimate        = ultimate,
    reserve         = reserve,
    total_reserve   = sum(reserve),
    total_ultimate  = sum(ultimate)
  )
}
