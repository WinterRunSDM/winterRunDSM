library(GA)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(winterRunDSM)
library(DSMCalibrationData)
library(DSMscenario)

source("calibration/update-params.R")

res <- read_rds("calibration/upper-sac-only-no-suggestions_2026-03-10_140235.rds")

# Print GA summary
summary(res)
cat("\nBest fitness:", res@fitnessValue, "\n")
cat("\nSolution vector:\n")
print(res@solution[1,])

# Run model with best solution
keep <- c(1)
r1_solution <- res@solution[1, ]
r1_params <- update_params(x = r1_solution, winterRunDSM::wr_sdm_baseline_params)
r1_params <- DSMCalibrationData::set_synth_years(r1_params)
r1_sim <- winter_run_model(seeds = DSMCalibrationData::grandtab_imputed$winter,
                           mode = "calibrate", ..params = r1_params, stochastic = FALSE)

# Compute per-watershed and overall correlations
r1_nat <- as_tibble(r1_sim[keep, , drop = FALSE]) |>
  mutate(watershed = DSMscenario::watershed_labels[keep]) |>
  gather(year, spawners, -watershed) |>
  mutate(type = "simulated", year = readr::parse_number(year) + 5)

r1_obs <- as_tibble(DSMCalibrationData::grandtab_observed$winter[keep, , drop = FALSE]) |>
  mutate(watershed = DSMscenario::watershed_labels[keep]) |>
  gather(year, spawners, -watershed) |>
  mutate(type = "observed", year = as.numeric(year) - 1997) |>
  filter(!is.na(spawners), year > 5)

r1_eval <- bind_rows(r1_nat, r1_obs)

cat("\n--- Per-watershed correlations ---\n")
r1_eval |> spread(type, spawners) |> filter(!is.na(observed)) |>
  group_by(watershed) |>
  summarise(r = cor(observed, simulated, use = "pairwise.complete.obs")) |>
  arrange(desc(abs(r))) |> print()

cat("\n--- Overall correlation ---\n")
r1_eval |> spread(type, spawners) |> filter(!is.na(observed)) |>
  summarise(r = cor(observed, simulated, use = "pairwise.complete.obs")) |> print()

# SSE from fitness
cat("\n--- SSE (fitness value, negated) ---\n")
cat(-res@fitnessValue, "\n")

# Time series comparison plot
p1 <- r1_eval |>
  ggplot(aes(year, spawners, color = type)) +
  geom_line() +
  facet_wrap(~watershed, scales = "free_y") +
  labs(title = "Observed vs Simulated Spawners",
       x = "Year", y = "Spawners") +
  theme_minimal()
print(p1)

# Observed vs predicted scatter plot
p2 <- r1_eval |>
  spread(type, spawners) |>
  filter(!is.na(observed)) |>
  ggplot(aes(observed, simulated)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~watershed, scales = "free") +
  labs(title = "Observed vs Simulated (1:1 line)",
       x = "Observed Spawners", y = "Simulated Spawners") +
  theme_minimal()
print(p2)
