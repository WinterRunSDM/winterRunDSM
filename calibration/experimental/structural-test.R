library(winterRunDSM)
library(DSMCalibrationData)
library(dplyr)
library(tidyr)
library(ggplot2)

source("calibration/update-params.R")

params <- DSMCalibrationData::set_synth_years(winterRunDSM::wr_sdm_baseline_params)
seeds <- DSMCalibrationData::grandtab_imputed$winter
obs <- DSMCalibrationData::grandtab_observed$winter[1, 6:20]

# Key parameters that most affect overall survival:
#  x[2]  = surv_juv_rear_int (repeated 31x)
#  x[7]  = surv_juv_delta_int
#  x[11] = ocean_entry_success_int
#  x[12] = surv_egg_to_fry_temp_effect

# Fix non-key params at mid-range values
base_x <- c(
  1.75,   # x1:  surv_adult_enroute_int
  0,      # x2:  surv_juv_rear_int        ** VARY **
  1.75,   # x3:  surv_juv_rear_contact_points
  1.75,   # x4:  surv_juv_rear_prop_diversions
  1.75,   # x5:  surv_juv_rear_total_diversions
  0,      # x6:  surv_juv_bypass_int
  0,      # x7:  surv_juv_delta_int        ** VARY **
  1.75,   # x8:  surv_juv_delta_contact_points
  1.75,   # x9:  surv_juv_delta_total_diverted
  0,      # x10: surv_juv_outmigration_sj_int
  -2.25,  # x11: ocean_entry_success_int   ** VARY **
  0.5     # x12: surv_egg_to_fry_temp_effect ** VARY **
)

# Grid over key parameters
rear_int_vals   <- seq(-3.5, 3.5, by = 1.75)
delta_int_vals  <- seq(-3.5, 3.5, by = 1.75)
ocean_int_vals  <- seq(-3.5, -1, by = 0.625)
egg_fry_vals    <- c(0.25, 0.5, 0.75, 1.0)

grid <- expand.grid(
  rear_int = rear_int_vals,
  delta_int = delta_int_vals,
  ocean_int = ocean_int_vals,
  egg_fry = egg_fry_vals
)

cat("Grid size:", nrow(grid), "combinations\n")

results <- data.frame()

for (i in 1:nrow(grid)) {
  x <- base_x
  x[2]  <- grid$rear_int[i]
  x[7]  <- grid$delta_int[i]
  x[11] <- grid$ocean_int[i]
  x[12] <- grid$egg_fry[i]

  p <- update_params(x, params)
  p <- DSMCalibrationData::set_synth_years(p)

  sim <- tryCatch(
    withCallingHandlers({
      winter_run_model(seeds = seeds, mode = "calibrate", ..params = p, stochastic = FALSE)
    }, warning = function(w) invokeRestart("muffleWarning")),
    error = function(e) NULL
  )

  if (is.null(sim)) next

  pred <- sim[1, ]
  r <- cor(pred, as.numeric(obs), use = "pairwise.complete.obs")
  sse <- sum((pred - as.numeric(obs))^2, na.rm = TRUE)
  ratio_early <- mean(pred[1:4]) / mean(as.numeric(obs)[1:4])

  results <- rbind(results, data.frame(
    i = i,
    rear_int = x[2], delta_int = x[7], ocean_int = x[11], egg_fry = x[12],
    r = r, sse = sse, ratio_early = ratio_early,
    pred_mean = mean(pred), obs_mean = mean(as.numeric(obs))
  ))

  if (i %% 100 == 0) cat("Done", i, "/", nrow(grid), "| best r so far:", round(max(results$r, na.rm = TRUE), 3), "\n")
}

cat("\n=== TOP 10 BY CORRELATION ===\n")
results |> arrange(desc(r)) |> head(10) |> print()

cat("\n=== TOP 10 BY SSE ===\n")
results |> arrange(sse) |> head(10) |> print()

cat("\n=== TOP 10 BY EARLY YEAR MATCH (ratio closest to 1) ===\n")
results |> mutate(early_err = abs(ratio_early - 1)) |> arrange(early_err) |> head(10) |> print()

# Plot best correlation result
best <- results |> arrange(desc(r)) |> slice(1)
x_best <- base_x
x_best[2]  <- best$rear_int
x_best[7]  <- best$delta_int
x_best[11] <- best$ocean_int
x_best[12] <- best$egg_fry

p_best <- update_params(x_best, params)
p_best <- DSMCalibrationData::set_synth_years(p_best)
sim_best <- winter_run_model(seeds = seeds, mode = "calibrate", ..params = p_best, stochastic = FALSE)

plot_df <- data.frame(
  year = 6:20,
  observed = as.numeric(obs),
  simulated = sim_best[1, ]
) |> pivot_longer(-year, names_to = "type", values_to = "spawners")

p1 <- ggplot(plot_df, aes(year, spawners, color = type)) +
  geom_line() + geom_point() +
  labs(title = paste0("Best correlation (r=", round(best$r, 3), ")"),
       subtitle = paste0("rear_int=", best$rear_int, " delta_int=", best$delta_int,
                         " ocean_int=", best$ocean_int, " egg_fry=", best$egg_fry),
       x = "Year", y = "Spawners") +
  theme_minimal()
print(p1)

# Plot best SSE result
best_sse <- results |> arrange(sse) |> slice(1)
x_sse <- base_x
x_sse[2]  <- best_sse$rear_int
x_sse[7]  <- best_sse$delta_int
x_sse[11] <- best_sse$ocean_int
x_sse[12] <- best_sse$egg_fry

p_sse <- update_params(x_sse, params)
p_sse <- DSMCalibrationData::set_synth_years(p_sse)
sim_sse <- winter_run_model(seeds = seeds, mode = "calibrate", ..params = p_sse, stochastic = FALSE)

plot_df2 <- data.frame(
  year = 6:20,
  observed = as.numeric(obs),
  simulated = sim_sse[1, ]
) |> pivot_longer(-year, names_to = "type", values_to = "spawners")

p2 <- ggplot(plot_df2, aes(year, spawners, color = type)) +
  geom_line() + geom_point() +
  labs(title = paste0("Best SSE (", round(best_sse$sse), ")"),
       subtitle = paste0("rear_int=", best_sse$rear_int, " delta_int=", best_sse$delta_int,
                         " ocean_int=", best_sse$ocean_int, " egg_fry=", best_sse$egg_fry),
       x = "Year", y = "Spawners") +
  theme_minimal()
print(p2)
