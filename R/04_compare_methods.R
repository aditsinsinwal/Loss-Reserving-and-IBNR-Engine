# ==============================================================================
# 04_compare_methods.R
#
# Utilities to compare reserve estimates across methods (CL vs BF vs Bootstrap)
# and across lines of business, and to flag reserve adequacy concerns.
# ==============================================================================

#' Build a side-by-side comparison of total ultimate/reserve estimates from
#' chain-ladder, BF, and bootstrap (mean) for a single line of business.
compare_methods_one_line <- function(cl, bf, boot, line_name) {
  boot_total <- boot$summary[boot$summary$AccidentYear == "TOTAL", ]
  data.frame(
    Line               = line_name,
    CL_Reserve         = round(cl$total_reserve, 0),
    BF_Reserve         = round(bf$total_reserve, 0),
    Bootstrap_Mean     = boot_total$BootMean,
    Bootstrap_P75      = boot_total$P75,
    Bootstrap_P90      = boot_total$P90,
    Bootstrap_CV       = boot_total$CV,
    CL_vs_BF_Pct_Diff  = round((cl$total_reserve - bf$total_reserve) / bf$total_reserve, 4)
  )
}

#' Combine the per-line comparison tables built by compare_methods_one_line()
#' into a single cross-line summary table, plus a grand total row.
combine_line_comparisons <- function(comparison_list) {
  combined <- do.call(rbind, comparison_list)
  totals <- data.frame(
    Line              = "ALL LINES",
    CL_Reserve        = sum(combined$CL_Reserve),
    BF_Reserve        = sum(combined$BF_Reserve),
    Bootstrap_Mean    = sum(combined$Bootstrap_Mean),
    Bootstrap_P75     = sum(combined$Bootstrap_P75),
    Bootstrap_P90     = sum(combined$Bootstrap_P90),
    Bootstrap_CV      = NA,
    CL_vs_BF_Pct_Diff = round((sum(combined$CL_Reserve) - sum(combined$BF_Reserve)) / sum(combined$BF_Reserve), 4)
  )
  rbind(combined, totals)
}

#' Simple reserve-adequacy flag: compares a carried/booked reserve assumption
#' (here, taken as the CL point estimate, but could be swapped for an actual
#' booked reserve) against the bootstrap distribution, and reports where it
#' falls percentile-wise. Useful talking point for "reserve adequacy" framing.
flag_reserve_adequacy <- function(boot, label = "") {
  total_sims <- boot$total_reserve_sims
  total_sims <- total_sims[!is.na(total_sims)]
  point <- boot$point_estimate$total_reserve

  percentile <- mean(total_sims <= point)
  cat(sprintf(
    "\n[%s] Chain-ladder point reserve of %s sits at the %.1fth percentile of the bootstrap distribution.\n",
    label, format(round(point, 0), big.mark = ","), 100 * percentile
  ))
  if (percentile < 0.40) {
    cat("  -> Point estimate is on the LOW side of the simulated range; consider holding closer to P50-P60 for adequacy margin.\n")
  } else if (percentile > 0.75) {
    cat("  -> Point estimate is on the HIGH side of the simulated range; reserves appear conservative relative to the model.\n")
  } else {
    cat("  -> Point estimate sits comfortably within the central range of model uncertainty.\n")
  }
  invisible(percentile)
}
