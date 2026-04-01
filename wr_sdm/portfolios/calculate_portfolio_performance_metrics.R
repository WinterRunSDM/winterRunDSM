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

portfolio_results <- p1_results
calculate_performance_metrics <- function(portfolio_res_obj, portfolio_param_obj) {

  output <- list(
    
    # SPAWNERS
    spawners_split = tibble::tibble(
      watershed = character(),
      scenario  = character(),
      sim_year  = numeric(),
      spawners  = numeric()
    ),
    spawners = tibble::tibble(
      sim_year  = numeric(),
      scenario  = character(),
      spawners  = numeric()
    ),
    mean_spawners = tibble::tibble(
      scenario      = character(),
      mean_spawners = numeric()
    ),
    
    # pHOS
    spawn_prop = tibble::tibble(
      watershed    = character(),
      sim_year     = numeric(),
      prop_natural = numeric()
    ),
    returns_split = tibble::tibble(
      watershed        = character(),
      scenario         = character(),
      sim_year         = numeric(),
      spawners         = numeric(),
      prop_natural     = numeric(),
      nat_returns      = numeric(),
      hatchery_returns = numeric()
    ),
    returns = tibble::tibble(
      scenario         = character(),
      sim_year         = numeric(),
      nat_returns      = numeric(),
      hatchery_returns = numeric(),
      spawners         = numeric(),
      phos             = numeric()
    ),
    mean_phos = tibble::tibble(
      scenario  = character(),
      mean_phos = numeric()
    ),
    
    # DECLINE
    decline = tibble::tibble(
      scenario              = character(),
      sim_year              = numeric(),
      spawners              = numeric(),
      decline               = numeric(),
      decline_threshold     = integer(),
      consecutive_decline   = numeric(),
      cat_decline           = numeric(),
      cat_decline_threshold = integer()
    ),
    max_cat_declines = tibble::tibble(
      scenario    = character(),
      max_decline = numeric(),
      cat_decline = numeric()
    ),
    
    # ABUNDANCE TABLE
    abund_table = tibble::tibble(
      metric    = character(),
      baseline  = numeric(),
      portfolio = numeric(),
      objective = character()
    ),
    
    # JUVENILES - UPPER SAC
    juvs_us = tibble::tibble(
      watershed = character(),
      scenario  = character(),
      year      = integer(),
      total_juv = numeric()
    ),
    mean_juv_us = tibble::tibble(
      scenario = character(),
      mean_juv = numeric()
    ),
    
    # JUVENILES AT CHIPPS
    juv_chipps = tibble::tibble(
      scenario = character(),
      year     = integer(),
      jac      = numeric()
    ),
    mean_jac = tibble::tibble(
      scenario = character(),
      mean_jac = numeric()
    ),
    
    # CRR
    crr = tibble::tibble(
      scenario         = character(),
      sim_year         = numeric(),
      nat_returns      = numeric(),
      hatchery_returns = numeric(),
      spawners         = numeric(),
      phos             = numeric(),
      crr              = numeric(),
      crr_over_one     = integer()
    ),
    crr_split = tibble::tibble(
      scenario         = character(),
      watershed        = character(),
      sim_year         = numeric(),
      nat_returns      = numeric(),
      hatchery_returns = numeric(),
      spawners         = numeric(),
      prop_natural     = numeric(),
      crr              = numeric(),
      crr_over_one     = integer()
    ),
    crr_summary = tibble::tibble(
      scenario = character(),
      mean_crr = numeric()
    ),
    
    # PRODUCTIVITY TABLE
    prod_table = tibble::tibble(
      metric    = character(),
      baseline  = numeric(),
      portfolio = numeric(),
      objective = character()
    ),
    
    # LIFE HISTORY DIVERSITY
    juvenile_size_ocean_entry = tibble::tibble(
      year        = integer(),
      size_or_age = factor(levels = c("s", "m", "l", "vl")),
      value       = numeric(),
      scenario    = character()
    ),
    annual_total = tibble::tibble(
      year             = integer(),
      scenario         = character(),
      total_juveniles  = numeric()
    ),
    shannon_di_size = tibble::tibble(
      year          = integer(),
      scenario      = character(),
      shannon_index = numeric()
    ),
    mean_shannon_di_size = tibble::tibble(
      scenario                  = character(),
      avg_annual_shannon_di_size = numeric()
    ),
    
    # DIVERSITY TABLE
    diversity_table = tibble::tibble(
      metric    = character(),
      baseline  = numeric(),
      portfolio = numeric(),
      objective = character()
    ),
    # POPULATIONS IN TRIBS - Spawners
    spawners_bc = tibble::tibble(
      watershed = character(),
      scenario  = character(),
      sim_year  = numeric(),
      spawners  = numeric()
    ),
    spawners_abv_shasta = tibble::tibble(
      watershed = character(),
      sim_year  = integer(),
      spawners  = numeric(),
      scenario  = character()
    ),
    spawners_tribs = tibble::tibble(
      scenario  = character(),
      sim_year  = integer(),
      spawners  = numeric()
    ),
    mean_spawners_in_tribs = tibble::tibble(
      scenario            = character(),
      mean_spawners_trib  = numeric()
    ),
    
    # POPULATIONS IN TRIBS - Habitat
    fry_habitat_abv_shasta = tibble::tibble(
      sim_year      = character(),
      portfolio           = numeric(),
      baseline      = numeric(),
      hab_diff_fry  = numeric(),
      hab_prop_fry  = numeric()
    ),
    juv_habitat_abv_shasta = tibble::tibble(
      sim_year      = character(),
      portfolio           = numeric(),
      baseline      = numeric(),
      hab_diff_juv  = numeric(),
      hab_prop_juv  = numeric()
    ),
    fp_habitat_abv_shasta = tibble::tibble(
      sim_year     = character(),
      portfolio          = numeric(),
      baseline     = numeric(),
      hab_diff_fp  = numeric(),
      hab_prop_fp  = numeric()
    ),
    spawn_habitat_abv_shasta = tibble::tibble(
      sim_year        = character(),
      portfolio             = numeric(),
      baseline        = numeric(),
      hab_diff_spawn  = numeric(),
      hab_prop_spawn  = numeric()
    ),
    habitat_diff = tibble::tibble(
      sim_year        = integer(),
      hab_diff_fp     = numeric(),
      hab_prop_fp     = numeric(),
      hab_diff_juv    = numeric(),
      hab_prop_juv    = numeric(),
      hab_diff_fry    = numeric(),
      hab_prop_fry    = numeric(),
      hab_diff_spawn  = numeric(),
      hab_prop_spawn  = numeric(),
      scenario        = character()
    ),
    rearing_prop = tibble::tibble(
      sim_year     = integer(),
      scenario     = character(),
      hab_prop_juv = numeric(),
      hab_prop_fry = numeric(),
      hab_prop_fp  = numeric(),
      rear_prop    = numeric()
    ),
    rearing_prop_summary = tibble::tibble(
      scenario       = character(),
      mean_rear_prop = numeric()
    ),
    habitat_abv_shasta = tibble::tibble(
      sim_year        = integer(),
      scenario        = character(),
      hab_diff_spawn  = numeric(),
      hab_prop_spawn  = numeric(),
      hab_diff_juv    = numeric(),
      hab_prop_juv    = numeric(),
      hab_diff_fp     = numeric(),
      hab_prop_fp     = numeric(),
      hab_diff_fry    = numeric(),
      hab_prop_fry    = numeric(),
      spawn           = integer(),
      rear            = integer(),
      habitat_score   = integer()
    ),
    mean_habitat_abv_shasta = tibble::tibble(
      scenario       = character(),
      habitat_access = numeric()
    ),
    
    # POPULATIONS IN TRIBS - Independent Populations
    ind_pop = tibble::tibble(
      scenario                  = character(),
      sim_year                  = numeric(),
      nat_returns               = numeric(),
      hatchery_returns          = numeric(),
      spawners                  = numeric(),
      phos                      = numeric(),
      crr                       = numeric(),
      crr_over_one              = integer(),
      decline                   = numeric(),
      decline_threshold         = integer(),
      consecutive_decline       = numeric(),
      cat_decline               = numeric(),
      cat_decline_threshold     = integer(),
      growth_rate =     numeric(),
      above_500_spawners        = integer(),
      phos_less_than_5_percent  = integer(),
      crr_above_1               = integer(),
      growth_rate_above_1       = integer(),
      independent_conditions    = integer()
    ),
    ind_pop_long = tibble::tibble(
      scenario  = character(),
      sim_year  = numeric(),
      metric    = character(),
      value     = integer()
    ),
    populations_table = tibble::tibble(
      metric    = character(),
      baseline  = numeric(),
      portfolio = numeric(),
      objective = character()
    ),
    metrics_table = tibble::tibble(
      objective = character(),
      metric    = character(),
      watershed = character(),
      baseline  = numeric(),
      portfolio = numeric()
    )
  )
## Abundance -------------------
### Spawners ----------------------------

# by watershed
spawners_split <- baseline_results$spawners |> 
  as.data.frame() |> 
  mutate(watershed = winterRunDSM::watershed_labels,
         scenario = "baseline") |> 
  bind_rows(portfolio_res_obj$spawners |> 
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


# ggplot(spawners) +
#   geom_line(aes(sim_year, spawners, color = scenario))

# summary 
mean_spawners <- spawners |> 
  group_by(scenario) |> 
  summarize(mean_spawners = round(mean(spawners))) |> 
  ungroup()

### pHOS -----------------
spawn_prop <- portfolio_res_obj$proportion_natural_at_spawning[c("Upper Sacramento River", "Battle Creek"),] |> 
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

### output update --------------------
output$spawners_split <- spawners_split
output$spawners <- spawners
output$mean_spawners <- mean_spawners
output$spawn_prop <- spawn_prop
output$returns_split <- returns_split
output$returns <- returns
output$mean_phos <- mean_phos
output$decline <- decline
output$max_cat_declines <- max_cat_declines
output$abund_table <- abund_table

## Productivity ------------------------
### Juveniles leaving Upper Sac -------------------
juvs_us <- baseline_results$juveniles |>
  mutate( scenario = "baseline") |>
  bind_rows(portfolio_res_obj$juveniles |> mutate(scenario = "portfolio")) |> 
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
  bind_rows(portfolio_res_obj$juveniles_at_chipps |> 
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

### output update --------
output$juvs_us <- juvs_us
output$mean_juv_us <- mean_juv_us
output$juv_chipps <- juv_chipps
output$mean_jac <- mean_jac
output$crr <- crr
output$crr_split <- crr_split
output$crr_summary <- crr_summary
output$prod_table <- prod_table

## Life History Diversity ------------------

### Size class diversity ---------------
juvenile_size_ocean_entry <- baseline_results$juveniles_at_chipps |>
  group_by(year, size_or_age = size) |>
  summarise(value = sum(juveniles_at_chipps, na.rm = TRUE)) |>
  mutate(scenario = "baseline") |> bind_rows(
    portfolio_res_obj$juveniles_at_chipps |>
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
### output updates ------------
output$juvenile_size_ocean_entry <- juvenile_size_ocean_entry
output$annual_total <- annual_total
output$shannon_di_size <- shannon_di_size
output$mean_shannon_di_size <- mean_shannon_di_size
output$diversity_table <- diversity_table

## Populations in tribs ------------------------
### Spawners in tribs------------------
spawners_bc <- spawners_split  |>  filter(watershed %in% c("Battle Creek"))

spawners_abv_shasta <- baseline_results$spawners_abv_dam |> 
  as.table() |> 
  as.data.frame() |> 
  mutate(scenario = "baseline") |> 
  bind_rows(portfolio_res_obj$spawners_abv_dam |> 
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
fry_habitat_abv_shasta <- portfolio_param_obj$inchannel_habitat_fry["Upper Sacramento River",,] |> 
  as.table() |> 
  as.data.frame() |> 
  mutate(watershed = "Upper Sacramento River", scenario = "portfolio") |> 
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
  mutate(hab_diff_fry = portfolio-baseline,
         hab_prop_fry = hab_diff_fry/(baseline + hab_diff_fry))

juv_habitat_abv_shasta <- portfolio_param_obj$inchannel_habitat_juvenile["Upper Sacramento River",,] |> 
  as.table() |> 
  as.data.frame() |> 
  mutate(watershed = "Upper Sacramento River", scenario = "portfolio") |> 
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
  mutate(hab_diff_juv = portfolio-baseline,
         hab_prop_juv = hab_diff_juv/(baseline+hab_diff_juv))

fp_habitat_abv_shasta <- portfolio_param_obj$floodplain_habitat["Upper Sacramento River",,] |> 
  as.table() |> 
  as.data.frame() |> 
  mutate(watershed = "Upper Sacramento River", scenario = "portfolio") |> 
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
  mutate(hab_diff_fp = portfolio-baseline,
         hab_prop_fp = hab_diff_fp/(baseline+hab_diff_fp))

# spawning 
spawn_habitat_abv_shasta <- portfolio_param_obj$spawning_habitat["Upper Sacramento River",,] |> 
  as.table() |> 
  as.data.frame() |> 
  mutate(watershed = "Upper Sacramento River", scenario = "portfolio") |> 
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
  mutate(hab_diff_spawn = portfolio-baseline,
         hab_prop_spawn = hab_diff_spawn/(baseline+hab_diff_spawn))

habitat_diff <- fp_habitat_abv_shasta |> select(sim_year, hab_diff_fp, hab_prop_fp) |> 
  left_join(juv_habitat_abv_shasta |> select(sim_year, hab_diff_juv, hab_prop_juv)) |> 
  left_join(fry_habitat_abv_shasta |> select(sim_year, hab_diff_fry, hab_prop_fry)) |> 
  left_join(spawn_habitat_abv_shasta|> select(sim_year, hab_diff_spawn, hab_prop_spawn)) |> 
  mutate(sim_year = as.integer(sim_year)) |> 
  mutate(scenario = "portfolio") |>
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



### Combine ----------------
populations_table <- mean_spawners_in_tribs |> 
  left_join(rearing_prop_summary) |> 
  left_join(mean_habitat_abv_shasta) |> 
  pivot_longer(cols = -scenario,
               names_to = "metric", 
               values_to = "value") |> 
  pivot_wider(names_from = scenario,
              values_from = value) |> 
  mutate(objective = "independent populations")

metrics_table <- abund_table |>
  bind_rows(prod_table,diversity_table,populations_table) |>
  mutate(watershed = "combined") |>
  select(objective, metric, watershed, baseline, portfolio)|>
  mutate(objective = factor(objective, levels = c("abundance", "productivity", "diversity and fitness", "independent populations"))) |>
  arrange(objective) 
  # left_join(obj_metrics) |>
  # select(Objective = objective_display,
  #        Metric = metric_display,
  #        Watershed = watershed,
  #        `Baseline Results` = baseline,
  #        `Portfolio Results` = portfolio) |>
  # mutate(`Baseline Results` = format_metric(`Baseline Results`),
  #        `Portfolio Results` = format_metric(`Portfolio Results`))

### output updates ------------------
output$spawners_bc <- spawners_bc
output$spawners_abv_shasta <- spawners_abv_shasta
output$spawners_tribs <- spawners_tribs
output$mean_spawners_in_tribs <- mean_spawners_in_tribs
output$fry_habitat_abv_shasta <- fry_habitat_abv_shasta
output$juv_habitat_abv_shasta <- juv_habitat_abv_shasta
output$fp_habitat_abv_shasta <- fp_habitat_abv_shasta
output$spawn_habitat_abv_shasta <- spawn_habitat_abv_shasta
output$habitat_diff <- habitat_diff
output$rearing_prop <- rearing_prop
output$rearing_prop_summary <- rearing_prop_summary
output$habitat_abv_shasta <- habitat_abv_shasta
output$mean_habitat_abv_shasta <- mean_habitat_abv_shasta
output$ind_pop <- ind_pop
output$ind_pop_long <- ind_pop_long
output$populations_table <- populations_table
output$metrics_table <- metrics_table

# Summary 
# summary_metrics_table <- bind_rows(metrics_table, watershed_metrics) |> 
#   mutate(objective = factor(objective, levels = c("abundance", "productivity", "diversity and fitness", "number of populations"))) |> 
#   arrange(objective) |> 
#   left_join(obj_metrics) |> 
#   select(Objective = objective_display,
#          Metric = metric_display,
#          Watershed = watershed,
#          `Baseline Results` = baseline,
#          `Portfolio Results` = portfolio) |> 
#   mutate(`Baseline Results` = format_metric(`Baseline Results`),
#          `Portfolio Results` = format_metric(`Portfolio Results`))

return(output)
}


