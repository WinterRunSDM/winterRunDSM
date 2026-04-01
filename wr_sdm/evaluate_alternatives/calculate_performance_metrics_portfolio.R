# Script to pair with summarize_outputs_scen_intro.qmd. 
# Adapt to have this work for all portfolios, or model others after this. 
# Creates table of all the performance metrics for portfolio. 
library(zoo)
library(dplyr)
library(tidyr)
library(purrr)
library(winterRunDSM)

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
# portfolio_params <- create_param_list(c("H-1", "H-2c", "SR-2b", "SR-4b",
#                                       "SR-8", "O-2", "ASD-3", "ASD-4", "ASD-5", "ASD-7"))
portfolio_params <- create_param_list(c("ASD-4","ASD-6", "ASD-7", "H-1"))

portfolio_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                                     mode = "seed",
                                                     seeds = NULL, 
                                                     ..params = portfolio_params)

portfolio_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                       ..params = portfolio_params,
                                                       seeds = portfolio_seeds)

# portfolio_results2 <- winterRunDSM::winter_run_model(mode = "simulate",
#                                               ..params = winterRunDSM::wr_sdm_scen_intro_params,
#                                               seeds = baseline_seeds)


#TODO

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
  bind_rows(portfolio_results$spawners |> 
              as.data.frame() |> 
              mutate(watershed = winterRunDSM::watershed_labels,
                     scenario = "portfolio")) |> 
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

ggplot(spawners) +
  geom_line(aes(sim_year, spawners, color = scenario))

# summary 
mean_spawners <- spawners |> 
  group_by(scenario) |> 
  summarize(mean_spawners = round(mean(spawners))) |> 
  ungroup()

### pHOS -----------------
spawn_prop <- portfolio_results$proportion_natural_at_spawning[c("Upper Sacramento River", "Battle Creek"),] |> 
  as.data.frame() |> 
  mutate(watershed = c("Upper Sacramento River", "Battle Creek")) |> 
  pivot_longer(`1`:`20`,
               names_to = "sim_year",
               values_to = "prop_natural") |> 
  mutate(sim_year = as.numeric(sim_year),
         prop_natural = replace(prop_natural, is.nan(prop_natural), 0))

returns_split <- left_join(spawners_split, spawn_prop) |> 
  mutate(nat_returns = spawners * prop_natural,
         hatchery_returns = spawners - nat_returns)

returns <- returns_split |> 
  group_by(scenario, sim_year) |> 
  summarize(nat_returns = sum(nat_returns),
            hatchery_returns = sum(hatchery_returns),
            spawners = sum(spawners)) |> 
  ungroup() |> 
  mutate(phos = hatchery_returns/spawners)

# mean phos
mean_phos <- returns |>
  group_by(scenario) |>
  summarize(mean_phos = round(mean(phos),2)) |> 
  ungroup()

### Decline -------------------------
decline <- spawners |>
  group_by(scenario) |>
  mutate(
    decline = (spawners - lag(spawners)) / lag(spawners),
    decline_threshold = if_else(decline <= -0.1, 1L, 0L, missing = 0L),
    consecutive_decline = accumulate(replace(
      decline_threshold, is.na(decline_threshold), 0
    ), ~ if (.y > 0)
      .x + 1
    else
      0),
    cat_decline = (spawners - lag(spawners, 3)) / lag(spawners, 3),
    cat_decline_threshold = if_else(cat_decline <= -0.9, 1L, 0L)
  ) |>
  ungroup() |>
  arrange(scenario, sim_year)

max_cat_declines <- decline |> 
  group_by(scenario) |> 
  summarize(max_decline = round(max(consecutive_decline)),
            cat_decline = sum(cat_decline_threshold, na.rm = TRUE)) |> 
  ungroup()

### Combined abundance metrics-----------------
abund_table <- mean_spawners |> 
  left_join(mean_spawners) |> left_join(mean_phos) |> 
  left_join(max_cat_declines) |> 
  pivot_longer(cols = mean_spawners:cat_decline,
               names_to = "metric", 
               values_to = "value") |> 
  pivot_wider(names_from = scenario,
              values_from = value) |> 
  mutate(objective = "abundance")

## Productivity ------------------------
### Juveniles leaving Upper Sac -------------------
juvs_us <- baseline_results$juveniles |>
  mutate( scenario = "baseline") |>
  bind_rows(portfolio_results$juveniles |> mutate(scenario = "portfolio")) |> 
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
  bind_rows(portfolio_results$juveniles_at_chipps |> 
              as.data.frame() |> 
              mutate(scenario = "portfolio")) |>  
  filter(watershed %in% c("Upper Sacramento River"),
         size %in% c("l", "vl")) |> 
  mutate(juveniles_at_chipps = replace(juveniles_at_chipps, is.nan(juveniles_at_chipps), 0)) |> 
  group_by(scenario, year) |> 
  summarize(jac = sum(juveniles_at_chipps)) |> 
  ungroup()

mean_jac <- juv_chipps |> 
  group_by(scenario) |> 
  summarize(mean_jac = round(mean(jac))) |> 
  ungroup()

### CRR -------------------------------
crr <-returns |> 
  group_by(scenario) |> 
  mutate(crr = lead(nat_returns,3)/spawners,
         crr_over_one = if_else(crr > 1, 1L, 0L)) |> 
  ungroup() |> 
  mutate(crr = replace(crr, is.nan(crr), NA))

crr_split <-returns_split |> 
  group_by(scenario, watershed) |> 
  mutate(crr = lead(nat_returns,3)/spawners,
         crr_over_one = if_else(crr > 1, 1L, 0L)) |> 
  ungroup() |> 
  mutate(crr = replace(crr, is.nan(crr), NA))

crr_summary <- crr |> 
  group_by(scenario) |> 
  summarize(mean_crr = round(mean(crr, na.rm = TRUE),3)) |> 
  ungroup() |> 
  mutate(mean_crr = replace(mean_crr, is.nan(mean_crr), NA))

### Combine ------------------
prod_table <- mean_juv_us |> 
  left_join(mean_jac) |> left_join(crr_summary) |>  
  pivot_longer(cols = -scenario,
               names_to = "metric", 
               values_to = "value") |> 
  pivot_wider(names_from = scenario,
              values_from = value) |> 
  mutate(objective = "productivity")


## Life History Diversity ------------------

### Size class diversity ---------------
juvenile_size_ocean_entry <- baseline_results$juveniles_at_chipps |>
  group_by(year, size_or_age = size) |>
  summarise(value = sum(juveniles_at_chipps, na.rm = TRUE)) |>
  mutate(scenario = "baseline") |> bind_rows(
    portfolio_results$juveniles_at_chipps |>
      group_by(year, size_or_age = size) |>
      summarise(value = sum(juveniles_at_chipps, na.rm = TRUE)) |>
      mutate(scenario = "portfolio")) |> 
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

### Combine ----------------
diversity_table <- mean_shannon_di_size |> 
  pivot_longer(cols = -scenario,
               names_to = "metric", 
               values_to = "value") |> 
  pivot_wider(names_from = scenario,
              values_from = value) |> 
  mutate(objective = "diversity and fitness")


## Populations in tribs ------------------------
### Spawners in tribs------------------
spawners_bc <- spawners_split  |>  filter(watershed %in% c("Battle Creek"))

spawners_abv_shasta <- baseline_results$spawners_abv_dam |> 
  as.table() |> 
  as.data.frame() |> 
  mutate(scenario = "baseline") |> 
  bind_rows(portfolio_results$spawners_abv_dam |> 
              as.table() |> 
              as.data.frame() |> 
              mutate(scenario = "portfolio")) |> 
  rename(watershed = Var1,
         sim_year = Var2,
         spawners = Freq) |> 
  filter(watershed == "Upper Sacramento River") |> 
  mutate(sim_year = as.integer(sim_year)) 

spawners_tribs <- spawners_bc |> 
  bind_rows(spawners_abv_shasta) |> 
  group_by(scenario, sim_year) |> 
  summarize(spawners = sum(spawners)) |> 
  ungroup() 

mean_spawners_in_tribs <- spawners_tribs |> 
  group_by(scenario) |> 
  summarize(mean_spawners_trib = round(mean(spawners)))

### Habitat proportion -----------------

# calculate difference between portfolio and baseline rearing habitat
fry_habitat_abv_shasta <- portfolio_params$inchannel_habitat_fry["Upper Sacramento River",,] |> 
  as.table() |> 
  as.data.frame() |> 
  mutate(watershed = "Upper Sacramento River", scenario = "alt") |> 
  bind_rows(wr_sdm_baseline_params$inchannel_habitat_fry["Upper Sacramento River",,] |> 
              as.table() |> 
              as.data.frame() |> 
              mutate(watershed = "Upper Sacramento River",
                     scenario = "baseline")) |> 
  rename(month = Var1, 
         sim_year= Var2,
         hab_sqm = Freq) |> 
  group_by(sim_year, scenario) |> 
  summarize(total_habitat = sum(hab_sqm)) |> 
  pivot_wider(names_from = "scenario", values_from = "total_habitat") |> 
  mutate(hab_diff_fry = alt-baseline,
         hab_prop_fry = hab_diff_fry/(baseline + hab_diff_fry))

juv_habitat_abv_shasta <- portfolio_params$inchannel_habitat_juvenile["Upper Sacramento River",,] |> 
  as.table() |> 
  as.data.frame() |> 
  mutate(watershed = "Upper Sacramento River", scenario = "alt") |> 
  bind_rows(wr_sdm_baseline_params$inchannel_habitat_juvenile["Upper Sacramento River",,] |> 
              as.table() |> 
              as.data.frame() |> 
              mutate(watershed = "Upper Sacramento River",
                     scenario = "baseline")) |> 
  rename(month = Var1, 
         sim_year= Var2,
         hab_sqm = Freq) |> 
  group_by(sim_year, scenario) |> 
  summarize(total_habitat = sum(hab_sqm)) |> 
  pivot_wider(names_from = "scenario", values_from = "total_habitat") |> 
  mutate(hab_diff_juv = alt-baseline,
         hab_prop_juv = hab_diff_juv/(baseline+hab_diff_juv))
  
fp_habitat_abv_shasta <- portfolio_params$floodplain_habitat["Upper Sacramento River",,] |> 
  as.table() |> 
  as.data.frame() |> 
  mutate(watershed = "Upper Sacramento River", scenario = "alt") |> 
  bind_rows(wr_sdm_baseline_params$floodplain_habitat["Upper Sacramento River",,] |> 
              as.table() |> 
              as.data.frame() |> 
              mutate(watershed = "Upper Sacramento River",
                     scenario = "baseline")) |> 
  rename(month = Var1, 
         sim_year= Var2,
         hab_sqm = Freq) |> 
  group_by(sim_year, scenario) |> 
  summarize(total_habitat = sum(hab_sqm)) |> 
  pivot_wider(names_from = "scenario", values_from = "total_habitat") |> 
  mutate(hab_diff_fp = alt-baseline,
         hab_prop_fp = hab_diff_fp/(baseline+hab_diff_fp))

# spawning 
spawn_habitat_abv_shasta <- portfolio_params$spawning_habitat["Upper Sacramento River",,] |> 
  as.table() |> 
  as.data.frame() |> 
  mutate(watershed = "Upper Sacramento River", scenario = "alt") |> 
  bind_rows(wr_sdm_baseline_params$spawning_habitat["Upper Sacramento River",,] |> 
              as.table() |> 
              as.data.frame() |> 
              mutate(watershed = "Upper Sacramento River",
                     scenario = "baseline")) |> 
  rename(month = Var1, 
         sim_year= Var2,
         hab_sqm = Freq) |> 
  group_by(sim_year, scenario) |> 
  summarize(total_habitat = sum(hab_sqm)) |> 
  pivot_wider(names_from = "scenario", values_from = "total_habitat") |> 
  mutate(hab_diff_spawn = alt-baseline,
         hab_prop_spawn = hab_diff_spawn/(baseline+hab_diff_spawn))

habitat_diff <- fp_habitat_abv_shasta |> select(sim_year, hab_diff_fp, hab_prop_fp) |> 
  left_join(juv_habitat_abv_shasta |> select(sim_year, hab_diff_juv, hab_prop_juv)) |> 
  left_join(fry_habitat_abv_shasta |> select(sim_year, hab_diff_fry, hab_prop_fry)) |> 
  left_join(spawn_habitat_abv_shasta|> select(sim_year, hab_diff_spawn, hab_prop_spawn)) |> 
  mutate(sim_year = as.integer(sim_year)) |> 
  mutate(scenario = "alt") |>
  bind_rows(data.frame(sim_year = 1:21,
                       hab_diff_fp = 0, hab_prop_fp = 0,
                       hab_diff_juv = 0, hab_prop_juv = 0,
                       hab_diff_fry = 0, hab_prop_fry = 0,
                       hab_diff_spawn = 0, hab_prop_spawn = 0,
                       scenario = "baseline"))
  
#### rearing habitat proportion ---------
rearing_prop <- habitat_diff |> 
  select(sim_year, scenario, hab_prop_juv, hab_prop_fry, hab_prop_fp) |> 
  group_by(sim_year, scenario) |> 
  mutate(rear_prop = mean(hab_prop_juv, hab_prop_fry, hab_prop_fp)) |> 
  ungroup() 

# rearing prop summary 
rearing_prop_summary <- rearing_prop |> 
  group_by(scenario) |> 
    summarize(mean_rear_prop = mean(rear_prop))


### Spawning and rearing habitat above Shasta-------------------
# Should this change by year? 
# abv_dam_rear_habitat_proportion > 0 can we create this? 

# summary by year
habitat_abv_shasta <- 
  habitat_diff |> 
  group_by(sim_year, scenario) |> 
  mutate(spawn = if_else(hab_diff_spawn >0, 1L, 0L),
         rear = if_else((hab_diff_juv>0 | hab_diff_fp > 0 | hab_diff_fry>0), 1L, 0L),
         habitat_score = sum(spawn, rear) )

mean_habitat_abv_shasta <- habitat_abv_shasta |> 
group_by(scenario) |> 
  summarize(habitat_access = mean(habitat_score)) |> 
  ungroup()
  

### Independent populations -----------------
ind_pop <- returns |> left_join(crr) |> left_join(decline) |> 
  mutate(growth_rate = (spawners-lag(spawners))/lag(spawners)) |> 
  mutate(above_500_spawners = if_else(spawners > 500, 1L, 0L),
         phos_less_than_5_percent = if_else(phos < .05, 1L, 0L),
         crr_above_1 = if_else(crr >= 1, 1L, 0L),
         growth_rate_above_1 = if_else(growth_rate >= 0, 1L, 0L),
         independent_conditions = if_else(above_500_spawners & phos_less_than_5_percent &
                                            growth_rate_above_1 & crr_above_1, 1L, 0L))

ind_pop_long <- ind_pop |> 
  pivot_longer(cols = c(above_500_spawners, phos_less_than_5_percent, crr_above_1, growth_rate_above_1), names_to = "metric", values_to= "value")

ind_dep_pop <- ind_pop |> 
  filter(!is.na(crr)) |> 
  filter(is.na(crr) | sim_year >= sort(unique(sim_year[!is.na(crr)]), decreasing = TRUE)[3]) |>
  group_by(scenario) |> 
  summarize(independent = if_else(sum(independent_conditions) == 3, 1L, 0L)) |> 
  group_by(scenario) |> 
  summarize(total_pops = length(unique(watershed)),
            ind_pop = sum(independent))

ind_pop_summary <- ind_dep_pop |> 
  select(scenario, ind_pop)

### Combine ----------------
populations_table <- mean_spawners_in_tribs |> 
  left_join(habitat_abv_shasta_summary) |> 
  left_join(ind_pop_summary) |> 
  pivot_longer(cols = -scenario,
               names_to = "metric", 
               values_to = "value") |> 
  pivot_wider(names_from = scenario,
              values_from = value) |> 
  mutate(objective = "independent populations")


## Metrics table ------------------
metrics_table <- abund_table |> 
  bind_rows(prod_table,diversity_table,populations_table) |> 
  mutate(watershed = "combined") |> 
  select(objective, metric, watershed, baseline, portfolio)

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
  select(objective, metric, watershed, baseline, portfolio)

# Summary 
summary_metrics_table <- bind_rows(metrics_table, watershed_metrics) |> 
  mutate(objective = factor(objective, levels = c("abundance", "productivity", "diversity and fitness", "number of populations"))) |> 
  arrange(objective) |> 
  left_join(obj_metrics) |> 
  select(Objective = objective_display,
         Metric = metric_display,
         Watershed = watershed,
         `Baseline Results` = baseline,
         `Portfolio Results` = portfolio) |> 
  mutate(`Baseline Results` = format_metric(`Baseline Results`),
         `Portfolio Results` = format_metric(`Portfolio Results`))
