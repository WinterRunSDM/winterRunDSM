library(GA)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(winterRunDSM)
library(DSMCalibrationData)
library(DSMscenario)

source("calibration/update-params.R")

res <- read_rds("calibration/res-22026-03-06.rds")

# --- 1. Check if parameters hit bounds ---
param_names <- c("surv_adult_enroute_int", "surv_juv_rear_int", "surv_juv_rear_contact_points",
                 "surv_juv_rear_prop_diversions", "surv_juv_rear_total_diversions",
                 "surv_juv_bypass_int", "surv_juv_delta_int", "surv_juv_delta_contact_points",
                 "surv_juv_delta_total_diverted", "surv_juv_outmigration_sj_int",
                 "ocean_entry_success_int", "surv_egg_to_fry_temp_effect")

map_params <- tibble("LTO_index" = c(1:16, 13),
                     "R2R_index" = c(2, 6, 7, 10,
                                     NA, NA, NA, NA,
                                     NA, 12, 1, 11,
                                     3, 4, 5, 9, 8)) |>
  mutate("LTO_mins" = c(rep(-3.5,10),0,-3.5,rep(0,4), 0),
         "LTO_maxes" = c(rep(3.5,9),-1,rep(3.5,6), 3.5)) |>
  arrange(R2R_index) |>
  filter(!is.na(R2R_index))

solution <- res@solution[1, ]

cat("=== PARAMETER BOUNDS CHECK ===\n")
bounds_df <- tibble(
  param = param_names,
  lower = map_params$LTO_mins,
  value = solution,
  upper = map_params$LTO_maxes,
  at_lower = abs(solution - map_params$LTO_mins) < 0.01,
  at_upper = abs(solution - map_params$LTO_maxes) < 0.01,
  pct_range = round((solution - map_params$LTO_mins) / (map_params$LTO_maxes - map_params$LTO_mins) * 100, 1)
)
print(bounds_df, n = 20)

# --- 2. Run model and compare predicted vs observed year-by-year ---
keep <- c(1)
r1_params <- update_params(x = solution, winterRunDSM::wr_sdm_baseline_params)
r1_params <- DSMCalibrationData::set_synth_years(r1_params)
r1_sim <- winter_run_model(seeds = DSMCalibrationData::grandtab_imputed$winter,
                           mode = "calibrate", ..params = r1_params, stochastic = FALSE)

cat("\n=== PREDICTED VALUES (Upper Sacramento) ===\n")
print(round(r1_sim[1, ]))

cat("\n=== OBSERVED VALUES (Upper Sacramento) ===\n")
obs <- DSMCalibrationData::grandtab_observed$winter
cat("Observed spawners:\n")
print(round(obs[1, ]))

# --- 3. Check what observed data looks like for fitness (cols 6:20) ---
cat("\n=== FITNESS FUNCTION COMPARISON (cols 6:20) ===\n")
cat("Observed Upper Sac (cols 6:20):\n")
print(obs[1, 6:20, drop = FALSE])

cat("\nPredicted Upper Sac (cols 1:15 of sim):\n")
print(round(r1_sim[1, 1:15]))

# --- 4. GA convergence info ---
cat("\n=== GA CONVERGENCE ===\n")
cat("Iterations run:", res@iter, "\n")
cat("Best fitness:", res@fitnessValue, "\n")
cat("Run (consecutive gens without improvement):", res@run, "\n")

# --- 5. Scatter plot: observed vs predicted ---
r1_nat <- as_tibble(r1_sim[keep, , drop = FALSE]) |>
  mutate(watershed = DSMscenario::watershed_labels[keep]) |>
  gather(year, simulated, -watershed) |>
  mutate(year = readr::parse_number(year) + 5)

r1_obs <- as_tibble(obs[keep, , drop = FALSE]) |>
  mutate(watershed = DSMscenario::watershed_labels[keep]) |>
  gather(year, observed, -watershed) |>
  mutate(year = as.numeric(year) - 1997) |>
  filter(!is.na(observed), year > 5)

r1_combined <- r1_nat |> left_join(r1_obs, by = c("watershed", "year")) |>
  filter(!is.na(observed))

p1 <- r1_combined |>
  ggplot(aes(observed, simulated)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~watershed, scales = "free") +
  labs(title = "Observed vs Predicted (res-2026-03-05)",
       x = "Observed Natural Spawners",
       y = "Predicted Natural Spawners")

print(p1)

p2 <- r1_combined |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = simulated, color = "Predicted")) +
  geom_line(aes(y = observed, color = "Observed")) +
  facet_wrap(~watershed, scales = "free_y") +
  labs(title = "Time Series: Observed vs Predicted",
       y = "Natural Spawners", x = "Year") +
  scale_color_manual(values = c("Predicted" = "blue", "Observed" = "red"))

print(p2)

