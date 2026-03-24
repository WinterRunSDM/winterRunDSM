library(winterRunDSM)
library(DSMCalibrationData)

params <- DSMCalibrationData::set_synth_years(winterRunDSM::wr_sdm_baseline_params)

cat("=== UPPER SACRAMENTO FLOWS (cms) by month and year ===\n")
cat("Months 9-12 (Sep-Dec) then 1-5 (Jan-May)\n\n")

for (yr in 1:19) {
  cat(sprintf("Year %2d (sim):", yr))
  for (mo in c(9:12, 1:5)) {
    iter_year <- if (mo %in% 1:5) yr + 1 else yr
    flow <- params$upper_sacramento_flows[mo, iter_year]
    # show which survival bracket
    bracket <- ifelse(flow <= 122, "LOW", ifelse(flow <= 303, "MED", "HIGH"))
    cat(sprintf("  %s=%.0f(%s)", month.abb[mo], flow, bracket))
  }
  cat("\n")
}

cat("\n=== MIGRATORY SURVIVAL per reach (surv_juv_outmigration_sac) ===\n")
for (yr in 1:19) {
  cat(sprintf("Year %2d:", yr))
  for (mo in c(9:12, 1:5)) {
    iter_year <- if (mo %in% 1:5) yr + 1 else yr
    flow <- params$upper_sacramento_flows[mo, iter_year]
    surv <- (flow <= 122) * 0.03 + (flow > 122 & flow <= 303) * 0.189 + (flow > 303) * 0.508
    cat(sprintf("  %.3f", surv))
  }
  cat("\n")
}
