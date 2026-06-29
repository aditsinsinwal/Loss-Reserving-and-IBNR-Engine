# ==============================================================================
# 01_chain_ladder.R
#
# Classic (volume-weighted) chain-ladder development method.
# ==============================================================================

#' Compute volume-weighted age-to-age development factors from a cumulative
#' loss triangle.
#'
#' @param triangle n x n matrix, cumulative losses, NA in unobserved cells
#' @param exclude  optional list of c(row, col) pairs of age-to-age cells to
#'                 exclude from the factor calc (for outlier diagnostics)
#' @return numeric vector of length (n-1): development factors age k -> k+1
compute_ldf <- function(triangle, exclude = NULL) {
  n <- nrow(triangle)
  ldf <- numeric(n - 1)

  for (k in 1:(n - 1)) {
    # All accident years that have both column k and column k+1 observed
    valid <- !is.na(triangle[, k]) & !is.na(triangle[, k + 1])

    if (!is.null(exclude)) {
      for (ex in exclude) {
        if (ex[2] == k) valid[ex[1]] <- FALSE
      }
    }

    num <- sum(triangle[valid, k + 1])
    den <- sum(triangle[valid, k])
    ldf[k] <- if (den > 0) num / den else NA_real_
  }
  names(ldf) <- paste0(k <- 1:(n - 1), "-", k + 1)
  ldf
}

#' Convert age-to-age factors into cumulative development factors (CDF),
#' i.e. the factor needed to take each age straight to ultimate.
#' @param ldf vector of age-to-age factors, length n-1
#' @return vector of length n: CDF to ultimate at each development age
#'         (CDF at the final age = 1, since it's treated as fully developed
#'         unless a tail factor is supplied)
compute_cdf <- function(ldf, tail_factor = 1.0) {
  n <- length(ldf) + 1
  cdf <- numeric(n)
  cdf[n] <- tail_factor
  for (k in (n - 1):1) {
    cdf[k] <- cdf[k + 1] * ldf[k]
  }
  names(cdf) <- paste0("age", 1:n)
  cdf
}

#' Run the full chain-ladder method on a triangle.
#'
#' @param triangle n x n cumulative loss triangle (NA = unobserved)
#' @param tail_factor multiplicative factor applied beyond the last observed
#'                     development age, to approximate losses beyond the
#'                     triangle's maturity (1.0 = assume fully developed)
#' @return list with ldf, cdf, latest diagonal, ultimate losses, reserves
chain_ladder <- function(triangle, tail_factor = 1.0) {
  n <- nrow(triangle)

  ldf <- compute_ldf(triangle)
  cdf <- compute_cdf(ldf, tail_factor = tail_factor)

  # Latest diagonal = most recent cumulative loss observed for each AY
  latest <- numeric(n)
  latest_dev_age <- numeric(n)
  for (i in 1:n) {
    obs_cols <- which(!is.na(triangle[i, ]))
    latest_dev_age[i] <- max(obs_cols)
    latest[i] <- triangle[i, latest_dev_age[i]]
  }
  names(latest) <- rownames(triangle)

  cdf_at_latest <- cdf[latest_dev_age]
  ultimate <- latest * cdf_at_latest
  reserve <- ultimate - latest

  list(
    method        = "Chain-Ladder",
    ldf           = ldf,
    cdf           = cdf,
    latest        = latest,
    latest_dev_age = latest_dev_age,
    ultimate      = ultimate,
    reserve       = reserve,
    total_reserve = sum(reserve),
    total_ultimate = sum(ultimate)
  )
}

#' Print a clean summary table for any reserving result object
#' (works for chain_ladder, bornhuetter_ferguson, and bootstrap summaries)
print_reserve_summary <- function(result, premium = NULL) {
  ay <- names(result$latest)
  df <- data.frame(
    AccidentYear   = ay,
    Latest         = round(result$latest, 0),
    Ultimate       = round(result$ultimate, 0),
    Reserve_IBNR   = round(result$reserve, 0)
  )
  if (!is.null(premium)) {
    df$ELR <- round(result$ultimate / premium[ay], 4)
  }
  totals <- data.frame(
    AccidentYear = "TOTAL",
    Latest = round(sum(result$latest), 0),
    Ultimate = round(sum(result$ultimate), 0),
    Reserve_IBNR = round(sum(result$reserve), 0)
  )
  if (!is.null(premium)) totals$ELR <- round(sum(result$ultimate) / sum(premium[ay]), 4)

  out <- rbind(df, totals)
  cat("\n===", result$method, "Summary ===\n")
  print(out, row.names = FALSE)
  invisible(out)
}
