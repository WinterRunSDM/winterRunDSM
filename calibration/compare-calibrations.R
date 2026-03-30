library(GA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(winterRunDSM)
library(DSMCalibrationData)
library(DSMscenario)

source("calibration/update-params.R")

# --- Load calibration results ---
# R2R 2021 calibration (11 params)
r2r_res <- read_rds("calibration/calibration-results-2021-10-11_125845.rds")

# LTO A5 calibration (12 params) — update path as needed
lto_res <- read_rds("calibration/upper-sac-only-no-suggestions_2026-03-10_140235.rds")

# --- Parameter comparison ---
# Shared param names (first 11 are common)
shared_names <- c("surv_adult_enroute_int", "surv_juv_rear_int", "surv_juv_rear_contact_points",
                  "surv_juv_rear_prop_diversions", "surv_juv_rear_total_diversions",
                  "surv_juv_bypass_int", "surv_juv_delta_int", "surv_juv_delta_contact_points",
                  "surv_juv_delta_total_diverted", "surv_juv_outmigration_sj_int",
                  "ocean_entry_success_int")

r2r_solution <- r2r_res@solution[1, ]
lto_solution <- lto_res@solution[1, ]

param_comparison <- tibble(
  param = shared_names,
  r2r_2021 = r2r_solution[1:11],
  lto_a5 = lto_solution[1:11],
  diff = lto_a5 - r2r_2021,
  pct_change = round((diff / abs(r2r_2021)) * 100, 1)
)

# Add the extra param (egg-to-fry temp effect) if present
if (length(lto_solution) >= 12) {
  param_comparison <- bind_rows(
    param_comparison,
    tibble(param = "surv_egg_to_fry_temp_effect",
           r2r_2021 = NA_real_,
           lto_a5 = lto_solution[12],
           diff = NA_real_,
           pct_change = NA_real_)
  )
}

param_comparison

# --- Fitness comparison ---
fitness_comparison <- tibble(
  calibration = c("R2R 2021", "LTO A5"),
  fitness = c(r2r_res@fitnessValue, lto_res@fitnessValue),
  sse = c(-r2r_res@fitnessValue, -lto_res@fitnessValue),
  iterations = c(r2r_res@iter, lto_res@iter)
)

fitness_comparison

# --- Side-by-side parameter bar chart ---
param_long <- param_comparison |>
  filter(!is.na(r2r_2021)) |>
  select(param, r2r_2021, lto_a5) |>
  pivot_longer(cols = c(r2r_2021, lto_a5), 
               names_to = "calibration", values_to = "value") |>
  mutate(calibration = ifelse(calibration == "r2r_2021", "R2R 2021", "LTO A5"))

ggplot(param_long, aes(x = reorder(param, value), y = value, fill = calibration)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Calibrated Parameter Comparison",
       x = NULL, y = "Parameter Value") +
  theme_minimal()

# --- Run both calibrations against observed GrandTab ---
# Watersheds to compare (old used 1,3; show Upper Sac at minimum)
keep <- c(1)

# Run current model with R2R 2021 calibration params
r2r_params <- winterRunDSM::wr_sdm_baseline_params
r2r_params$..surv_adult_enroute_int <- r2r_solution[1]
r2r_params$..surv_juv_rear_int <- rep(r2r_solution[2], 31)
r2r_params$..surv_juv_rear_contact_points <- r2r_solution[3]
r2r_params$..surv_juv_rear_prop_diversions <- r2r_solution[4]
r2r_params$..surv_juv_rear_total_diversions <- r2r_solution[5]
r2r_params$..surv_juv_bypass_int <- r2r_solution[6]
r2r_params$..surv_juv_delta_int <- r2r_solution[7]
r2r_params$..surv_juv_delta_contact_points <- r2r_solution[8]
r2r_params$..surv_juv_delta_total_diverted <- r2r_solution[9]
r2r_params$..surv_juv_outmigration_sj_int <- r2r_solution[10]
r2r_params$..ocean_entry_success_int <- rep(r2r_solution[11], 31)
r2r_params <- DSMCalibrationData::set_synth_years(r2r_params)

r2r_sim <- winter_run_model(seeds = DSMCalibrationData::grandtab_imputed$winter,
                            mode = "calibrate", ..params = r2r_params, stochastic = FALSE)

# Run current model with LTO A5 calibration params
lto_params <- update_params(x = lto_solution, winterRunDSM::wr_sdm_baseline_params)
lto_params <- DSMCalibrationData::set_synth_years(lto_params)

lto_sim <- winter_run_model(seeds = DSMCalibrationData::grandtab_imputed$winter,
                            mode = "calibrate", ..params = lto_params, stochastic = FALSE)

# Observed GrandTab data
obs <- DSMCalibrationData::grandtab_observed$winter

# Build comparison dataframe
build_sim_df <- function(sim, label, keep) {
  as_tibble(sim[keep, , drop = FALSE]) |>
    mutate(watershed = DSMscenario::watershed_labels[keep]) |>
    pivot_longer(-watershed, names_to = "year", values_to = "spawners") |>
    mutate(year = readr::parse_number(year) + 5, source = label)
}

obs_df <- as_tibble(obs[keep, , drop = FALSE]) |>
  mutate(watershed = DSMscenario::watershed_labels[keep]) |>
  pivot_longer(-watershed, names_to = "year", values_to = "spawners") |>
  mutate(year = as.numeric(year) - 1997, source = "Observed (GrandTab)") |>
  filter(!is.na(spawners), year > 5)

all_df <- bind_rows(
  build_sim_df(r2r_sim, "R2R 2021", keep),
  build_sim_df(lto_sim, "LTO A5", keep),
  obs_df
)

# --- Time series plot ---
ggplot(all_df, aes(year, spawners, color = source, linetype = source)) +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(values = c("Observed (GrandTab)" = "solid",
                                   "R2R 2021" = "dashed",
                                   "LTO A5" = "dashed")) +
  scale_color_manual(values = c("Observed (GrandTab)" = "black",
                                "R2R 2021" = "blue",
                                "LTO A5" = "red")) +
  facet_wrap(~watershed, scales = "free_y") +
  labs(title = "Observed vs Simulated Spawners: R2R 2021 vs LTO A5",
       x = "Year", y = "Spawners", color = NULL, linetype = NULL) +
  theme_minimal()

# --- Scatter: simulated vs observed for each calibration ---
scatter_df <- bind_rows(
  build_sim_df(r2r_sim, "R2R 2021", keep),
  build_sim_df(lto_sim, "LTO A5", keep)
) |>
  inner_join(obs_df |> select(watershed, year, observed = spawners), by = c("watershed", "year"))

ggplot(scatter_df, aes(observed, spawners, color = source)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  facet_wrap(~watershed, scales = "free") +
  labs(title = "Observed vs Simulated (1:1 line)",
       x = "Observed Spawners", y = "Simulated Spawners", color = NULL) +
  theme_minimal()

# --- Per-calibration correlation with observed ---
scatter_df |>
  group_by(source, watershed) |>
  summarise(r = cor(observed, spawners, use = "pairwise.complete.obs"),
            rmse = sqrt(mean((spawners - observed)^2, na.rm = TRUE)),
            .groups = "drop")
