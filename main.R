# ==============================================================================
# main.R
#
# Loss Reserving & IBNR Engine
# Runs chain-ladder, Bornhuetter-Ferguson, and ODP bootstrap reserving across
# multiple P&C lines of business (CAS Schedule P-style triangles), and
# produces:
#   - per-line reserve summaries for each method
#   - a cross-line, cross-method comparison table
#   - bootstrap-based reserve ranges (P10/P50/P75/P90/P99) and CVs
#   - CSV exports of every result table to output/
#
# Usage:
#   Rscript main.R
# ==============================================================================

suppressWarnings(suppressMessages({
  script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
  if (is.null(script_dir) || script_dir == "") script_dir <- getwd()
}))

source(file.path(script_dir, "R", "00_simulate_schedule_p.R"))
source(file.path(script_dir, "R", "01_chain_ladder.R"))
source(file.path(script_dir, "R", "02_bornhuetter_ferguson.R"))
source(file.path(script_dir, "R", "03_bootstrap_reserving.R"))
source(file.path(script_dir, "R", "04_compare_methods.R"))
source(file.path(script_dir, "R", "05_validate_against_truth.R"))

dir.create(file.path(script_dir, "output"), showWarnings = FALSE)

N_YEARS <- 10
N_BOOT  <- 1000
TAIL_FACTOR <- 1.0   # set > 1.0 to assume development beyond the triangle

cat("================================================================\n")
cat(" LOSS RESERVING & IBNR ENGINE\n")
cat(" Chain-Ladder | Bornhuetter-Ferguson | ODP Bootstrap\n")
cat("================================================================\n")

lines <- build_schedule_p_lines(n_years = N_YEARS)

results <- list()
comparisons <- list()

for (key in names(lines)) {
  ln <- lines[[key]]
  cat("\n----------------------------------------------------------------\n")
  cat("LINE OF BUSINESS:", ln$line, "\n")
  cat("----------------------------------------------------------------\n")

  cl   <- chain_ladder(ln$triangle, tail_factor = TAIL_FACTOR)
  bf   <- bornhuetter_ferguson(ln$triangle, ln$premium, elr = NULL, tail_factor = TAIL_FACTOR)
  boot <- bootstrap_reserve(ln$triangle, n_boot = N_BOOT, tail_factor = TAIL_FACTOR, seed = 1000 + which(names(lines) == key))

  cl_df <- print_reserve_summary(cl, premium = ln$premium)
  bf_df <- print_reserve_summary(bf, premium = ln$premium)

  cat("\n--- ODP Bootstrap reserve range (total) ---\n")
  print(boot$summary[boot$summary$AccidentYear == "TOTAL", ], row.names = FALSE)

  flag_reserve_adequacy(boot, label = ln$line)

  cat("\n--- Validation vs. known simulated truth (sanity check only) ---\n")
  val_df <- validate_against_truth(ln, cl, bf)
  print(val_df, row.names = FALSE)

  results[[key]] <- list(line = ln, cl = cl, bf = bf, boot = boot)
  comparisons[[key]] <- compare_methods_one_line(cl, bf, boot, ln$line)

  # Per-line CSV exports (reuse the data frames already built above)
  write.csv(cl_df, file.path(script_dir, "output", paste0(key, "_chain_ladder.csv")), row.names = FALSE)
  write.csv(bf_df, file.path(script_dir, "output", paste0(key, "_bornhuetter_ferguson.csv")), row.names = FALSE)
  write.csv(boot$summary, file.path(script_dir, "output", paste0(key, "_bootstrap_summary.csv")), row.names = FALSE)
  write.csv(val_df, file.path(script_dir, "output", paste0(key, "_validation_vs_truth.csv")), row.names = FALSE)
}

cat("\n================================================================\n")
cat(" CROSS-LINE / CROSS-METHOD COMPARISON\n")
cat("================================================================\n")
combined <- combine_line_comparisons(comparisons)
print(combined, row.names = FALSE)
write.csv(combined, file.path(script_dir, "output", "cross_line_comparison.csv"), row.names = FALSE)

cat("\nAll output CSVs written to:", file.path(script_dir, "output"), "\n")
cat("Run complete.\n")
