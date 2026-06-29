# ==============================================================================
# 05_validate_against_truth.R
#
# Since the triangles are SIMULATED, we actually know the "true" generating
# ultimate loss for each accident year (which a real reserving analyst never
# gets to see). This script backtests chain-ladder and BF against that truth,
# which is a useful way to sanity-check the engine and illustrate method
# bias/accuracy -- NOT something you can do with real Schedule P data, but
# valuable here as a code-correctness and methodology check.
# ==============================================================================

#' Compare CL and BF ultimate estimates to the known true ultimate losses
#' for a simulated line of business.
#' @param ln output of simulate_triangle() (must include true_ultimate)
#' @param cl output of chain_ladder()
#' @param bf output of bornhuetter_ferguson()
validate_against_truth <- function(ln, cl, bf) {
  ay <- names(ln$true_ultimate)
  df <- data.frame(
    AccidentYear  = ay,
    TrueUltimate  = round(ln$true_ultimate, 0),
    CL_Ultimate   = round(cl$ultimate[ay], 0),
    BF_Ultimate   = round(bf$ultimate[ay], 0),
    CL_Error_Pct  = round((cl$ultimate[ay] - ln$true_ultimate) / ln$true_ultimate, 4),
    BF_Error_Pct  = round((bf$ultimate[ay] - ln$true_ultimate) / ln$true_ultimate, 4)
  )
  totals <- data.frame(
    AccidentYear = "TOTAL",
    TrueUltimate = round(sum(ln$true_ultimate), 0),
    CL_Ultimate  = round(sum(cl$ultimate[ay]), 0),
    BF_Ultimate  = round(sum(bf$ultimate[ay]), 0),
    CL_Error_Pct = round((sum(cl$ultimate[ay]) - sum(ln$true_ultimate)) / sum(ln$true_ultimate), 4),
    BF_Error_Pct = round((sum(bf$ultimate[ay]) - sum(ln$true_ultimate)) / sum(ln$true_ultimate), 4)
  )
  rbind(df, totals)
}
