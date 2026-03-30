# run-calibration-enhanced.R
# Same as run-calibration.R but uses enhanced proxy year indices
# that incorporate Shasta storage and Keswick temperature matching.

library(winterRunDSM)
library(GA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(DSMCalibrationData)

source("calibration/fitness.R")
source("calibration/update-params.R")

# Load enhanced indices from synthetic-redo.R output
enhanced_spawn_index <- readr::read_rds(here::here("data-raw", "enhanced_spawn_index.rds"))
enhanced_year_index  <- readr::read_rds(here::here("data-raw", "enhanced_year_index.rds"))

run_calibration_enhanced <- function(description, pop_size = 100, iter = 5000, seed = 1234) {

  params <- DSMCalibrationData::set_synth_years(
    winterRunDSM::wr_sdm_baseline_params,
    spawn_years = enhanced_spawn_index,
    years = enhanced_year_index
  )
  grandtab_observed <- DSMCalibrationData::grandtab_observed

  map_params <- tibble::tibble(
    "LTO_index" = c(1:16, 13),
    "R2R_index" = c(2, 6, 7, 10,
                    NA, NA, NA, NA,
                    NA, 11, 1, 12,
                    3, 4, 5, 9, 8)) |>
    dplyr::mutate(
      "LTO_mins" = c(rep(-3.5, 10), 0, -3.5, rep(0, 4), 0),
      "LTO_maxes" = c(rep(3.5, 9), -1, rep(3.5, 6), 3.5),
      "LTO_suggested" = c(-0.6558315, -3.4999845,  1.4933417, -3.0188308,
                           2.0000003,  0.7999889, -3.5000000, -0.1999996,
                          -3.4999920, -2.9839253,  3.4999976,  0.6466230,
                           0.0194795,  0.1000000,  0.3000000,  0.4820249,
                           0.0194795)) |>
    dplyr::arrange(R2R_index) |>
    dplyr::filter(!is.na(R2R_index))

  lower_bounds <- map_params$LTO_mins
  upper_bounds <- map_params$LTO_maxes
  lower_bounds[12] <- 0.01
  upper_bounds[12] <- 1

  LTO_suggestions_matrix <- matrix(map_params$LTO_suggested, nrow = 1, ncol = 12)

  set.seed(seed)

  monitor_fn <- function(obj) {
    cat("Gen:", obj@iter, " Best fitness:", obj@fitnessValue, "\n")
    best <- obj@population[which.max(obj@fitness), ]
    p <- update_params(x = best, params)
    p <- DSMCalibrationData::set_synth_years(p,
      spawn_years = enhanced_spawn_index,
      years = enhanced_year_index
    )
    sim <- winter_run_model(seeds = DSMCalibrationData::grandtab_imputed$winter,
                            mode = "calibrate", ..params = p, stochastic = FALSE)
    obs <- grandtab_observed$winter[1, 6:20]
    plot(1:15, sim[1, ], type = "o", col = "blue", pch = 16, ylim = range(c(sim[1, ], obs), na.rm = TRUE),
         xlab = "Year", ylab = "Spawners",
         main = paste0("Enhanced | Gen ", obj@iter, " | SSE: ", round(-obj@fitnessValue)))
    lines(1:15, obs, type = "o", col = "red", pch = 16)
    legend("topright", legend = c("Simulated", "Observed"), col = c("blue", "red"), lty = 1, pch = 16)
  }

  res <- ga(type = "real-valued",
            fitness =
              function(x) -winter_run_fitness(
                known_adults = grandtab_observed$winter,
                seeds = DSMCalibrationData::grandtab_imputed$winter,
                params = params,
                x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
                x[11], x[12]
              ),
            lower = lower_bounds,
            upper = upper_bounds,
            suggestions = LTO_suggestions_matrix,
            popSize = pop_size,
            maxiter = iter,
            run = 100,
            parallel = 4,
            pmutation = .5,
            monitor = monitor_fn)

  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  filename <- paste0("calibration/res-enhanced-", description, "_", timestamp, ".rds")
  readr::write_rds(res, filename)
  cat("Results saved to:", filename, "\n")

  return(res)
}

res_enhanced <- run_calibration_enhanced("enhanced-synth-years", pop_size = 200)

# Evaluate Results ------------------------------------
keep <- c(1)
r1_solution <- res_enhanced@solution[1, ]

r1_params <- update_params(x = r1_solution, winterRunDSM::wr_sdm_baseline_params)
r1_params <- DSMCalibrationData::set_synth_years(r1_params,
  spawn_years = enhanced_spawn_index,
  years = enhanced_year_index
)
r1_sim <- winter_run_model(seeds = DSMCalibrationData::grandtab_imputed$winter, mode = "calibrate",
                           ..params = r1_params,
                           stochastic = FALSE)

r1_nat_spawners <- as_tibble(r1_sim[keep, , drop = F]) |>
  mutate(watershed = DSMscenario::watershed_labels[keep]) |>
  gather(year, spawners, -watershed) |>
  mutate(type = "simulated",
         year = readr::parse_number(year) + 5)

r1_observed <- as_tibble((1 - winterRunDSM::wr_sdm_baseline_params$proportion_hatchery[keep]) * DSMCalibrationData::grandtab_observed$winter[keep, ]) |>
  mutate(watershed = DSMscenario::watershed_labels[keep]) |>
  gather(year, spawners, -watershed) |>
  mutate(type = "observed", year = as.numeric(year) - 1997) |>
  filter(!is.na(spawners),
         year > 5)

r1_eval_df <- bind_rows(r1_nat_spawners, r1_observed)

r1_eval_df |>
  ggplot(aes(year, spawners, color = type)) + geom_line() + facet_wrap(~watershed, scales = "free_y") +
  labs(title = "Enhanced Synthetic Years: Observed vs Simulated")

r1_eval_df |>
  spread(type, spawners) |>
  ggplot(aes(observed, simulated)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Enhanced Synthetic Years: Observed vs Predicted",
       x = "Observed Natural Spawners",
       y = "Predicted Natural Spawners") +
  xlim(0, 20000) +
  ylim(0, 20000)

r1_eval_df |>
  spread(type, spawners) |>
  filter(!is.na(observed)) |>
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )
