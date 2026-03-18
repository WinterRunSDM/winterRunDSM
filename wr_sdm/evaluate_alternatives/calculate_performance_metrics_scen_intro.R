# Script to pair with summarize_outputs_scen_intro.qmd. 
# Adapt to have this work for all portfolios, or model others after this. 
# Creates table of all the performance metrics for portfolio. 

# Run models ----------------------------------------------------

# baseline
baseline_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                                 mode = "seed",
                                                 seeds = NULL, 
                                                 ..params = winterRunDSM::wr_sdm_baseline_params)

baseline_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                   ..params = winterRunDSM::wr_sdm_baseline_params,
                                                   seeds = baseline_seeds)

# test scenario 
portfolio_eg_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                                     mode = "seed",
                                                     seeds = NULL, 
                                                     ..params = winterRunDSM::wr_sdm_scen_intro_params)

portfolio_eg_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                       ..params = winterRunDSM::wr_sdm_scen_intro_params,
                                                       seeds = portfolio_eg_seeds)
# portfolio_eg_results <- winterRunDSM::winter_run_model(mode = "simulate", 
#                                               ..params = winterRunDSM::wr_sdm_scen_intro_params,
#                                               seeds = baseline_seeds)


#TODO

# Why are there 2 spawners in years 1:3 for Battle Creek baseline? 
# Something weird with juveniles in years 5, 9, 13, 15, 19 with Battle Creek juveniles and pHOS (had to change to 0), could be to do with natural_proportion_with_renat
# Why does reducing harvest decrease spawners and juveniles?

# Calculate performance metrics -----------

## Abundance -------------------
### Spawners ----------------------------

# by watershed
spawners_split <- baseline_results$spawners |> 
  as.data.frame() |> 
  mutate(watershed = winterRunDSM::watershed_labels,
         scenario = "baseline") |> 
  bind_rows(portfolio_eg_results$spawners |> 
              as.data.frame() |> 
              mutate(watershed = winterRunDSM::watershed_labels,
                     scenario = "portfolio_eg")) |> 
  pivot_longer(`1`:`20`,
               names_to = "sim_year",
               values_to = "spawners") |> 
  filter(watershed %in% c("Upper Sacramento River", "Battle Creek")) |> 
  mutate(sim_year = as.numeric(sim_year),
         # Figure this part out
         spawners = replace(spawners, watershed == "Battle Creek" & scenario == "baseline", 0)) 

# combined
spawners <- spawners_split |> 
  group_by(sim_year, scenario) |> 
  summarize(spawners = sum(spawners))

# summary 
mean_spawners <- spawners |> 
  group_by(scenario) |> 
  summarize(mean_spawners = round(mean(spawners))) |> 
  ungroup()

### Natural and Hatchery Proportion Spawners -----------------
spawn_prop <- portfolio_eg_results$proportion_natural_at_spawning[c("Upper Sacramento River", "Battle Creek"),] |> 
  as.data.frame() |> 
  mutate(watershed = c("Upper Sacramento River", "Battle Creek")) |> 
  pivot_longer(`1`:`20`,
               names_to = "sim_year",
               values_to = "prop_natural") |> 
  mutate(sim_year = as.numeric(sim_year))

returns_split <- left_join(spawners_split, spawn_prop) |> 
  mutate(nat_returns = spawners * prop_natural,
         hatchery_returns = spawners - nat_returns)

returns <- returns_split |> 
  group_by(scenario, sim_year) |> 
  summarize(nat_returns = sum(nat_returns),
            hatchery_returns = sum(hatchery_returns))

# natural
mean_natural_return <- returns |>
  group_by(scenario) |>
  summarize(mean_natural_return = round(mean(nat_returns))) |> 
  ungroup()

# hatchery
mean_hatchery_return <- returns |>
  group_by(scenario) |>
  summarize(mean_hatchery_return = round(mean(hatchery_returns))) |> 
  ungroup()

### Decline -------------------------
decline <- spawners |> 
  group_by(scenario) |> 
  mutate(decline = if_else(spawners < lag(spawners,1), 1L, 0L)) |> 
  ungroup()

total_declines <- decline |> 
  group_by(scenario) |> 
  summarize(total_declines = round(sum(decline, na.rm = TRUE))) |> 
  ungroup()

### Catastrophic decline ------------------

change_spawn_split <- spawners_split |> 
  group_by(scenario, watershed) |> 
  mutate(change_abund = spawners-lag(spawners, 1),
         prop_change_abund = change_abund/lag(spawners,1)) |> 
  mutate(avg_change = rollapply(change_abund, width = 3, FUN = mean, align = "right", partial = FALSE, fill = NA),
         avg_prop_change = rollapply(prop_change_abund, width = 3, FUN = mean, align = "right", partial = FALSE, fill = NA) ) |> 
  ungroup()

change_spawn <- spawners |> 
  group_by(scenario) |> 
  mutate(change_abund = spawners-lag(spawners, 1),
         prop_change_abund = change_abund/lag(spawners,1)) |> 
  mutate(avg_change = rollapply(change_abund, width = 3, FUN = mean, align = "right", partial = FALSE, fill = NA),
         avg_prop_change = rollapply(prop_change_abund, width = 3, FUN = mean, align = "right", partial = FALSE, fill = NA) ) |> 
  ungroup()

max_declines <- change_spawn |> 
  group_by(scenario) |> 
  summarize(max_decline = round(min(avg_prop_change, na.rm = TRUE),3))

### Combined abundance metrics-----------------
abund_table <- mean_spawners |> 
  left_join(mean_natural_return) |> left_join(mean_hatchery_return) |> 
  left_join(total_declines) |> left_join(max_declines) |> 
  pivot_longer(cols = mean_spawners:max_decline,
               names_to = "metric", 
               values_to = "value") |> 
  pivot_wider(names_from = scenario,
              values_from = value) |> 
  mutate(objective = "abundance")

## Productivity ------------------------
### Juveniles leaving Upper Sac -------------------
juvs_us <- baseline_results$juveniles |>
  mutate( scenario = "baseline") |>
  bind_rows(portfolio_eg_results$juveniles |> mutate(scenario = "portfolio_eg")) |> 
  filter(watershed %in% c("Upper Sacramento River")) |> 
  group_by(watershed, scenario, year) |> 
  summarize(total_juv = sum(juveniles))

mean_juv_us <- juvs_us |> 
  group_by(scenario) |> 
  summarize(mean_juv = mean(total_juv)) |> 
  ungroup()

### Juveniles emigrating to ocean ------------------
juv_chipps <- baseline_results$juveniles_at_chipps |> 
  as.data.frame() |> 
  mutate(scenario = "baseline") |> 
  bind_rows(portfolio_eg_results$juveniles_at_chipps |> 
              as.data.frame() |> 
              mutate(scenario = "portfolio_eg")) |>  
  filter(watershed %in% c("Upper Sacramento River"),
         size %in% c("l", "vl")) |> 
  group_by(scenario, year) |> 
  summarize(jac = sum(juveniles_at_chipps)) |> 
  ungroup()

prop_nat_juv_at_chipps <- tibble("year" = 1:20,
                                 "prop_nat_jac" = baseline_results$proportion_natural_juves_in_tribs["Upper Sacramento River",],
                                 scenario = "baseline") |> 
  bind_rows(tibble("year" = 1:20,
                   "prop_nat_jac" = portfolio_eg_results$proportion_natural_juves_in_tribs["Upper Sacramento River",],
                   scenario = "portfolio_eg"))

nat_juv_at_chipps <- juv_chipps |> 
  left_join(prop_nat_juv_at_chipps, 
            by = c("year", "scenario")) |> 
  mutate(nat_jac = jac * prop_nat_jac)

mean_nat_jac <- nat_juv_at_chipps |> 
  group_by(scenario) |> 
  summarize(mean_nat_jac = round(mean(nat_jac))) |> 
  ungroup()

mean_jac <- juv_chipps |> 
  group_by(scenario) |> 
  summarize(mean_jac = round(mean(jac))) |> 
  ungroup()

### CRR -------------------------------

crr <-returns_split |> 
  group_by(scenario, watershed) |> 
  mutate(crr = lead(nat_returns,3)/spawners,
         # crr = replace(crr, watershed == "Battle Creek" & scenario == "baseline", 0),
         crr_over_one = if_else(crr > 1, 1L, 0L)) |> 
  ungroup() |> 
  mutate(crr = replace(crr, is.nan(crr), NA))

crr_summary <- crr |> 
  group_by(scenario, watershed) |> 
  summarize(years_over_one = sum(crr_over_one, na.rm = TRUE),
            mean_crr = round(mean(crr, na.rm = TRUE),3)) |> 
  ungroup() |> 
  mutate(mean_crr = replace(mean_crr, is.nan(mean_crr), NA))

### Combine ------------------
prod_table <- mean_juv_us |> 
  left_join(mean_jac) |>  
  pivot_longer(cols = -scenario,
               names_to = "metric", 
               values_to = "value") |> 
  pivot_wider(names_from = scenario,
              values_from = value) |> 
  mutate(objective = "productivity")


## Life History Diversity ------------------

### pHOS ----------------
phos <- baseline_results$phos |> 
  as.data.frame() |> 
  mutate(watershed = winterRunDSM::watershed_labels,
         scenario = "baseline") |> 
  bind_rows(portfolio_eg_results$phos |> 
              as.data.frame() |> 
              mutate(watershed = winterRunDSM::watershed_labels,
                     scenario = "portfolio_eg")) |> 
  pivot_longer(`1`:`20`,
               names_to = "sim_year",
               values_to = "phos") |> 
  filter(watershed %in% c("Upper Sacramento River", "Battle Creek")) |> 
  mutate(sim_year = as.numeric(sim_year),
         phos = replace(phos, watershed == "Battle Creek" & scenario == "baseline", NA))

phos_summary <- phos |> 
  group_by(scenario, watershed) |> 
  summarize(mean_phos = mean(phos, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(mean_phos = replace(mean_phos, is.nan(mean_phos), NA))

### Size class diversity ---------------
juvenile_size_ocean_entry <- baseline_results$juveniles_at_chipps |>
  group_by(year, watershed, size_or_age = size) |>
  summarise(value = sum(juveniles_at_chipps, na.rm = TRUE)) |>
  mutate(scenario = "baseline") |> bind_rows(
    portfolio_eg_results$juveniles_at_chipps |>
      group_by(year, watershed, size_or_age = size) |>
      summarise(value = sum(juveniles_at_chipps, na.rm = TRUE)) |>
      mutate(scenario = "portfolio_eg")) |> 
  filter(watershed %in% c("Upper Sacramento River")) |> 
  mutate(size_or_age = factor(size_or_age, levels = c("s", "m", "l", "vl")))

annual_total <- juvenile_size_ocean_entry |>
  group_by(year, scenario) |>
  summarize(total_juveniles = sum(value, na.rm = T))

shannon_di_size <- juvenile_size_ocean_entry |>
  group_by(year, size_or_age, scenario) |>
  summarize(frequency = sum(value, na.rm = T)) |>
  ungroup() |>
  left_join(annual_total) |>
  mutate(pi = frequency / total_juveniles,
         ln_pi = log(pi),
         pi_ln_pi = pi * ln_pi) |>
  group_by(year, scenario) |>
  summarize(shannon_index = -1 * sum(pi_ln_pi, na.rm = T)) |>
  ungroup()

mean_shannon_di_size <- shannon_di_size |>
  group_by(scenario) |>
  summarise(avg_annual_shannon_di_size = round(mean(shannon_index),2)) |> 
  ungroup()

### Timing diversity-------------

juvenile_month_ocean_entry <- baseline_results$juveniles_at_chipps |>
  group_by(year, watershed, month) |>
  summarise(value = sum(juveniles_at_chipps, na.rm = TRUE)) |>
  mutate(scenario = "baseline") |> bind_rows(
    portfolio_eg_results$juveniles_at_chipps |>
      group_by(year, watershed, month) |>
      summarise(value = sum(juveniles_at_chipps, na.rm = TRUE)) |>
      mutate(scenario = "portfolio_eg")) |> 
  filter(watershed %in% c("Upper Sacramento River"))

annual_total <- juvenile_month_ocean_entry |>
  group_by(year, scenario) |>
  summarize(total_juveniles = sum(value, na.rm = T))

shannon_di_timing <- juvenile_month_ocean_entry |>
  group_by(year, month, scenario) |>
  summarize(frequency = sum(value, na.rm = T)) |>
  ungroup() |>
  left_join(annual_total) |>
  mutate(pi = frequency / total_juveniles,
         ln_pi = log(pi),
         pi_ln_pi = pi * ln_pi) |>
  group_by(year, scenario) |>
  summarize(shannon_index = -1 * sum(pi_ln_pi, na.rm = T)) |>
  ungroup()

mean_shannon_di_timing <- shannon_di_timing |>
  group_by(scenario) |>
  summarise(avg_annual_shannon_di_timing = round(mean(shannon_index),2)) |> 
  ungroup() 


### Combine ----------------
diversity_table <- mean_shannon_di_size |> 
  left_join(mean_shannon_di_timing) |> 
  pivot_longer(cols = -scenario,
               names_to = "metric", 
               values_to = "value") |> 
  pivot_wider(names_from = scenario,
              values_from = value) |> 
  mutate(objective = "diversity and fitness")


## Populations in tribs ------------------------

### Spawners in tribs------------------
spawners_in_tribs <- spawners_split  |>  filter(watershed == "Battle Creek")

mean_spawners_in_tribs <- spawners_in_tribs |> 
  group_by(scenario) |> 
  summarize(mean_spawners_trib = round(mean(spawners)))

### Juveniles in tribs -----------------
juvs_trib <- baseline_results$juveniles |>
  mutate(scenario = "baseline") |>
  bind_rows(portfolio_eg_results$juveniles |> mutate(scenario = "portfolio_eg")) |> 
  filter(watershed %in% c("Battle Creek")) |> 
  group_by(watershed, scenario, year) |> 
  summarize(total_juv = sum(juveniles)) |> 
  ungroup() |> 
  mutate(total_juv = replace(total_juv, watershed=="Battle Creek"  & scenario == "baseline", 0))

mean_juv_trib <- juvs_trib |> 
  group_by(scenario) |> 
  summarize(mean_juv_trib = mean(total_juv))

### Independent populations -----------------
ind_pop <- phos |> left_join(crr) |> left_join(change_spawn_split) |> 
  mutate(growth_rate = change_abund/lag(spawners)) |> 
  mutate(above_500_spawners = if_else(spawners > 500, 1L, 0L),
         phos_less_than_5_percent = if_else(phos < .05, 1L, 0L),
         crr_above_1 = if_else(crr >= 1, 1L, 0L),
         growth_rate_above_1 = if_else(growth_rate >= 0, 1L, 0L),
         independent_conditions = if_else(above_500_spawners & phos_less_than_5_percent &
                                            growth_rate_above_1 & crr_above_1, 1L, 0L))

ind_pop_long <- ind_pop |> 
  pivot_longer(cols = c(above_500_spawners, phos_less_than_5_percent, crr_above_1, growth_rate_above_1), names_to = "metric", values_to= "value")

ind_dep_pop <- ind_pop |> 
  # filter(!is.na(crr)) |> 
  filter(is.na(crr) | sim_year >= sort(unique(sim_year[!is.na(crr)]), decreasing = TRUE)[3]) |>
  group_by(scenario, watershed) |> 
  summarize(independent = if_else(sum(independent_conditions) == 3, 1L, 0L)) |> 
  group_by(scenario) |> 
  summarize(total_pops = length(unique(watershed)),
            ind_pop = sum(independent),
            dep_pop = total_pops-ind_pop)

ind_pop_summary <- ind_dep_pop |> 
  select(scenario, ind_pop)


### Independent populations in Historic Habitat --------------------
# This is currently manual

ind_pop_historic <- data.frame(scenario = c("baseline", "portfolio_eg"),
                               ind_pop_historic = c(0,0))

### Dependent populations --------------------
dep_pop_summary <- ind_dep_pop |> 
  select(scenario, dep_pop)

### Combine ----------------
populations_table <- mean_spawners_in_tribs |> 
  left_join(mean_juv_trib) |> 
  left_join(ind_pop_historic) |> 
  left_join(ind_pop_summary) |> 
  left_join(dep_pop_summary) |> 
  pivot_longer(cols = -scenario,
               names_to = "metric", 
               values_to = "value") |> 
  pivot_wider(names_from = scenario,
              values_from = value) |> 
  mutate(objective = "number of populations")

## Metrics table ------------------
metrics_table <- abund_table |> 
  bind_rows(prod_table,diversity_table,populations_table) |> 
  mutate(watershed = "combined") |> 
  select(objective, metric, watershed, baseline, portfolio_eg)

watershed_metrics <- crr_summary |> left_join(phos_summary) |> 
  pivot_longer(cols = -c(scenario,watershed),
               names_to = "metric", 
               values_to = "value") |> 
  pivot_wider(names_from = scenario,
              values_from = value) |> 
  filter(metric != "years_over_one")  |> 
  mutate(objective = case_when(metric == "mean_crr" ~ "productivity",
                               metric == "mean_phos" ~ "diversity and fitness",
                               metric %in% c("independence_years", "dependence_years") ~ "number of populations")) |> 
  select(objective, metric, watershed, baseline, portfolio_eg)

# Summary 
summary_metrics_table <- bind_rows(metrics_table, watershed_metrics) |> 
  mutate(objective = factor(objective, levels = c("abundance", "productivity", "diversity and fitness", "number of populations"))) |> 
  arrange(objective) |> 
  left_join(obj_metrics) |> 
  select(Objective = objective_display,
         Metric = metric_display,
         Watershed = watershed,
         `Baseline Results` = baseline,
         `Portfolio Results` = portfolio_eg) |> 
  mutate(`Baseline Results` = format_metric(`Baseline Results`),
         `Portfolio Results` = format_metric(`Portfolio Results`))
