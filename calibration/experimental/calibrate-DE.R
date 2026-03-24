library(winterRunDSM)
library(DEoptim)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(DSMCalibrationData)
library(DSMscenario)

source("calibration/fitness.R")
source("calibration/update-params.R")

params <- DSMCalibrationData::set_synth_years(winterRunDSM::wr_sdm_baseline_params)
grandtab_observed <- DSMCalibrationData::grandtab_observed

# Parameter bounds (mapped from LTO to R2R indices)
map_params <- tibble("LTO_index" = c(1:16, 13),
                     "R2R_index" = c(2, 6, 7, 10,
                                     NA, NA, NA, NA,
                                     NA, 11, 1, 12,
                                     3, 4, 5, 9, 8)) |>
  mutate("LTO_mins" = c(rep(-3.5, 10), 0, -3.5, rep(0, 4), 0),
         "LTO_maxes" = c(rep(3.5, 9), -1, rep(3.5, 6), 3.5),
         "LTO_suggested" = c(-0.6558315, -3.4999845,  1.4933417, -3.0188308,
                              2.0000003,  0.7999889, -3.5000000, -0.1999996,
                             -3.4999920, -2.9839253,  3.4999976,  0.6466230,
                              0.0194795,  0.1000000,  0.3000000,  0.4820249,
                              0.0194795)) |>
  arrange(R2R_index) |>
  filter(!is.na(R2R_index))

lower_bounds <- map_params$LTO_mins
upper_bounds <- map_params$LTO_maxes
lower_bounds[12] <- 0.01
upper_bounds[12] <- 1

# Initial population seeded with LTO suggestions
LTO_suggestions_matrix <- NULL

# Perform calibration with DEoptim
set.seed(1234)

res <- DEoptim(
  fn = function(x) {
    winter_run_fitness(
      known_adults = grandtab_observed$winter,
      seeds = DSMCalibrationData::grandtab_imputed$winter,
      params = params,
      x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
      x[11], x[12]
    )
  },
  lower = lower_bounds,
  upper = upper_bounds,
  control = DEoptim.control(
    NP = 120,
    itermax = 5000,
    F = 0.8,
    CR = 0.9,
    strategy = 2,
    # initialpop = LTO_suggestions_matrix,
    trace = 1,
    reltol = 1e-8,
    steptol = 100
  )
)

readr::write_rds(res, paste0("calibration/res-DE-", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".rds"))

# Plot convergence
plot(res, plot.type = "bestvalit", type = "l")

# Evaluate Results ------------------------------------
keep <- c(1)
r1_solution <- res$optim$bestmem

r1_params <- update_params(x = r1_solution, winterRunDSM::wr_sdm_baseline_params)
r1_params <- DSMCalibrationData::set_synth_years(r1_params)
r1_sim <- winter_run_model(seeds = DSMCalibrationData::grandtab_imputed$winter, mode = "calibrate",
                           ..params = r1_params, stochastic = FALSE)

r1_nat_spawners <- as_tibble(r1_sim[keep, , drop = FALSE]) |>
  mutate(watershed = DSMscenario::watershed_labels[keep]) |>
  gather(year, spawners, -watershed) |>
  mutate(type = "simulated", year = readr::parse_number(year) + 5)

r1_observed <- as_tibble(DSMCalibrationData::grandtab_observed$winter[keep, , drop = FALSE]) |>
  mutate(watershed = DSMscenario::watershed_labels[keep]) |>
  gather(year, spawners, -watershed) |>
  mutate(type = "observed", year = as.numeric(year) - 1997) |>
  filter(!is.na(spawners), year > 5)

r1_eval_df <- bind_rows(r1_nat_spawners, r1_observed)

r1_eval_df |>
  ggplot(aes(year, spawners, color = type)) +
  geom_line() + geom_point() +
  facet_wrap(~watershed, scales = "free_y") +
  labs(title = "Observed vs Simulated Spawners (DEoptim)",
       x = "Year", y = "Spawners") +
  theme_minimal()

r1_eval_df |>
  spread(type, spawners) |>
  filter(!is.na(observed)) |>
  ggplot(aes(observed, simulated)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Observed vs Simulated (1:1 line)",
       x = "Observed Spawners", y = "Simulated Spawners") +
  theme_minimal()

cat("\n--- Correlation ---\n")
r1_eval_df |>
  spread(type, spawners) |>
  filter(!is.na(observed)) |>
  summarise(r = cor(observed, simulated, use = "pairwise.complete.obs")) |>
  print()

cat("\n--- SSE ---\n")
cat(res$optim$bestval, "\n")

