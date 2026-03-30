library(GA)
library(tidyr)
library(dplyr)
library(readr)
library(winterRunDSM)
library(DSMCalibrationData)

source("calibration/update-params.R")

res <- read_rds("calibration/res-22026-03-06.rds")
r1_solution <- res@solution[1, ]
r1_params <- update_params(x = r1_solution, winterRunDSM::wr_sdm_baseline_params)
r1_params <- DSMCalibrationData::set_synth_years(r1_params)

# --- Seed values (adults input each year) for Upper Sac (watershed 1) ---
seeds <- DSMCalibrationData::grandtab_imputed$winter
cat("=== SEED VALUES (Upper Sac, all years) ===\n")
print(seeds[1, ])

# --- Egg-to-fry survival by year ---
cat("\n=== EGG-TO-FRY SURVIVAL (Upper Sac, by year) ===\n")
for (yr in 1:19) {
  s <- surv_egg_to_fry(
    proportion_natural = 1 - r1_params$proportion_hatchery,
    scour = r1_params$prob_nest_scoured,
    .proportion_natural = r1_params$.surv_egg_to_fry_proportion_natural,
    .scour = r1_params$.surv_egg_to_fry_scour,
    .surv_egg_to_fry_int = r1_params$.surv_egg_to_fry_int,
    ..surv_egg_to_fry_mean_egg_temp_effect = r1_params$..surv_egg_to_fry_mean_egg_temp_effect
  )
  cat(sprintf("Year %2d: %.4f\n", yr, s[1]))
}

# --- Degree days / prespawn survival by year ---
cat("\n=== PRESPAWN SURVIVAL (Upper Sac, by year) ===\n")
for (yr in 1:19) {
  accumulated_degree_days <- cbind(
    jan = rowSums(r1_params$degree_days[, 1:4, yr]),
    feb = rowSums(r1_params$degree_days[, 2:4, yr]),
    march = rowSums(r1_params$degree_days[, 3:4, yr]),
    april = r1_params$degree_days[, 4, yr]
  )
  avg_dd <- apply(accumulated_degree_days, 1, weighted.mean, r1_params$month_return_proportions)
  ps <- surv_adult_prespawn(avg_dd,
                            .adult_prespawn_int = r1_params$.adult_prespawn_int,
                            .deg_day = r1_params$.adult_prespawn_deg_day)
  cat(sprintf("Year %2d: %.4f  (avg degree days: %.1f)\n", yr, ps[1], avg_dd[1]))
}

# --- Juveniles produced per year (spawn_success output) ---
cat("\n=== JUVENILES PRODUCED (Upper Sac, by year) ===\n")
for (yr in 1:19) {
  natural_proportion_with_renat <- 1 - r1_params$proportion_hatchery
  egg_to_fry_surv <- surv_egg_to_fry(
    proportion_natural = natural_proportion_with_renat,
    scour = r1_params$prob_nest_scoured,
    .proportion_natural = r1_params$.surv_egg_to_fry_proportion_natural,
    .scour = r1_params$.surv_egg_to_fry_scour,
    .surv_egg_to_fry_int = r1_params$.surv_egg_to_fry_int,
    ..surv_egg_to_fry_mean_egg_temp_effect = r1_params$..surv_egg_to_fry_mean_egg_temp_effect
  )
  
  accumulated_degree_days <- cbind(
    jan = rowSums(r1_params$degree_days[, 1:4, yr]),
    feb = rowSums(r1_params$degree_days[, 2:4, yr]),
    march = rowSums(r1_params$degree_days[, 3:4, yr]),
    april = r1_params$degree_days[, 4, yr]
  )
  avg_dd <- apply(accumulated_degree_days, 1, weighted.mean, r1_params$month_return_proportions)
  prespawn_survival <- surv_adult_prespawn(avg_dd,
                                           .adult_prespawn_int = r1_params$.adult_prespawn_int,
                                           .deg_day = r1_params$.adult_prespawn_deg_day)
  
  min_spawn_habitat <- apply(r1_params$spawning_habitat[, 1:4, yr], 1, min)
  
  init_adults <- round(seeds[, yr] * (1 - r1_params$natural_adult_removal_rate))
  
  default_hatch_age_dist <- tibble(watershed = winterRunDSM::watershed_labels,
                                   prop_2 = rep(.3, 31), prop_3 = rep(.6, 31),
                                   prop_4 = rep(.1, 31), prop_5 = rep(0, 31))
  default_nat_age_dist <- tibble(watershed = winterRunDSM::watershed_labels,
                                 prop_2 = rep(.22, 31), prop_3 = rep(.47, 31),
                                 prop_4 = rep(.26, 31), prop_5 = rep(.05, 31))
  
  juveniles <- spawn_success(
    escapement = init_adults,
    proportion_natural = natural_proportion_with_renat,
    hatchery_age_distribution = default_hatch_age_dist,
    natural_age_distribution = default_nat_age_dist,
    fecundity_lookup = r1_params$fecundity_lookup,
    adult_prespawn_survival = prespawn_survival,
    adult_prespawn_survival_abv_dam = .95,
    abv_dam_spawn_proportion = r1_params$above_dam_spawn_proportion,
    egg_to_fry_survival = egg_to_fry_surv,
    prob_scour = r1_params$prob_nest_scoured,
    spawn_habitat = min_spawn_habitat,
    sex_ratio = r1_params$spawn_success_sex_ratio,
    redd_size = r1_params$spawn_success_redd_size,
    fecundity = r1_params$spawn_success_fecundity,
    stochastic = FALSE
  )
  
  cat(sprintf("Year %2d: seeds=%6.0f  init_adults=%6.0f  total_juves=%10.0f  prespawn=%.3f  egg2fry=%.4f  spawn_hab=%.0f\n",
              yr, seeds[1, yr], init_adults[1], sum(juveniles[1, ]), prespawn_survival[1], egg_to_fry_surv[1], min_spawn_habitat[1]))
}

# --- Adults in ocean per year (final count entering ocean) ---
cat("\n=== MODEL OUTPUT: calculated_adults[1, ] (Upper Sac) ===\n")
r1_sim <- winter_run_model(seeds = seeds, mode = "calibrate",
                           ..params = r1_params, stochastic = FALSE)
cat("Cols 1-15 (years 6-20):\n")
print(round(r1_sim[1, ]))

cat("\n=== OBSERVED (Upper Sac, cols 6:20) ===\n")
print(DSMCalibrationData::grandtab_observed$winter[1, 6:20])
