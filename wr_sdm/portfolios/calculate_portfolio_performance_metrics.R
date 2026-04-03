# Script to pair with summarize_outputs_scen_intro.qmd. 
# Adapt to have this work for all portfolios, or model others after this. 
# Creates table of all the performance metrics for portfolio. 
library(zoo)
library(dplyr)
library(tidyr)
library(purrr)
library(winterRunDSM)

obj_metrics <- readxl::read_excel("wr_sdm/documentation/objectives_metrics_v2.xlsx")

format_metric <- function(x) {
  dplyr::case_when(
    is.na(x)        ~ "NA",
    abs(x) >= 1000  ~ formatC(x, format = "f", digits = 0, big.mark = ","),
    abs(x) >= 2   ~ formatC(x, format = "f", digits = 0),
    abs(x) >= 1     ~ formatC(x, format = "f", digits = 2),
    abs(x) > 0      ~ formatC(x, format = "f", digits = 3),
    TRUE            ~ formatC(x, format = "f", digits = 0)
  )
}

# Run models ----------------------------------------------------

# baseline
baseline_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                                 mode = "seed",
                                                 seeds = NULL, 
                                                 ..params = winterRunDSM::wr_sdm_baseline_params)

baseline_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                   ..params = winterRunDSM::wr_sdm_baseline_params,
                                                   seeds = baseline_seeds)

## Above-dam population breakdown ----------------

# Step 1: Get habitat proportions for each sub-area from portfolio params
# These mirror the logic in calculate_habitat_additions_ASD_BC
get_abv_dam_habitat_props <- function(portfolio_param_obj) {
  
  baseline_spawn <- wr_sdm_baseline_params$spawning_habitat["Upper Sacramento River",,2:21]
  portfolio_spawn <- portfolio_param_obj$spawning_habitat["Upper Sacramento River",,2:21]
  
  upper_sac_w_mccloud_spawn <- DSMhabitat::wr_spawn$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,2:21]
  upper_and_little_sac_spawn <- DSMhabitat::wr_spawn$action_5_upper_sac_tmh["Upper Sacramento River",,2:21]
  
  mccloud_addition_raw <- upper_sac_w_mccloud_spawn - upper_and_little_sac_spawn
  mccloud_spawn <- mccloud_spawn <- mccloud_addition_raw * wr_sdm_temp_habitat_scaling_factors$`Full McCloud River`$spawn
  
  little_sac_spawn <- (upper_and_little_sac_spawn - baseline_spawn) * 
    wr_sdm_temp_habitat_scaling_factors$`Little Sacramento River`$spawn
  
  # sum across months first, then we have a vector of length 20 (one per year)
  mccloud_annual <- colSums(mccloud_spawn)
  little_sac_annual <- colSums(little_sac_spawn)
  baseline_annual <- colSums(baseline_spawn)
  total_new_annual <- mccloud_annual + little_sac_annual
  total_annual <- total_new_annual + baseline_annual
  
  props <- tibble::tibble(
    sim_year           = 1:20,
    prop_mccloud = mccloud_annual / total_annual,
    prop_little_sac    = little_sac_annual    / total_annual,
    prop_upper_sac_blw = 1 - (mccloud_annual + little_sac_annual) / total_annual
  ) |>
    mutate(across(starts_with("prop_"), ~ replace(.x, is.nan(.x), 0)))
  
  return(props)
}


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
    abv_dam_habitat_props = tibble::tibble(
      sim_year           = integer(),
      prop_mccloud = numeric(),
      prop_little_sac    = numeric(),
      prop_upper_sac_blw = numeric()
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
    
    # Tributary
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
      mean_juv_rear_trib = numeric()
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
      num_trib = numeric()
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
      Objective = character(),
      Metric    = character(),
      Watershed = character(),
      watershed = character(),
      `Baseline Results`  = character(),
      `Portfolio Results` = character()
      
    ),
    
    # Output list initialization entries:
    
    sub_area_habitat_props = tibble::tibble(
      sim_year           = integer(),
      prop_mccloud = numeric(),
      prop_little_sac    = numeric(),
      prop_upper_sac_blw = numeric()
    ),
    spawners_abv_dam_split = tibble::tibble(
      sim_year               = integer(),
      spawners_abv_dam       = numeric(),
      prop_mccloud     = numeric(),
      prop_little_sac        = numeric(),
      prop_upper_sac_blw     = numeric(),
      spawners_mccloud = numeric(),
      spawners_little_sac    = numeric(),
      spawners_upper_sac_blw = numeric()
    ),
    sub_area_spawners = tibble::tibble(
      sim_year         = integer(),
      sub_area         = character(),
      spawners         = numeric(),
      prop_natural     = numeric(),
      nat_returns      = numeric(),
      hatchery_returns = numeric(),
      phos             = numeric()
    ),
    sub_area_ind_pop = tibble::tibble(
      sub_area                  = character(),
      sim_year                  = integer(),
      spawners                  = numeric(),
      prop_natural              = numeric(),
      nat_returns               = numeric(),
      hatchery_returns          = numeric(),
      phos                      = numeric(),
      crr                       = numeric(),
      growth_rate               = numeric(),
      decline                   = numeric(),
      cat_decline               = numeric(),
      above_500_spawners        = integer(),
      phos_less_than_5_percent  = integer(),
      crr_above_1               = integer(),
      growth_rate_above_1       = integer(),
      independent_conditions    = integer()
    ),
    spawn_prop_us = tibble::tibble(
      sim_year     = integer(),
      prop_natural = numeric()
    ),
    spawners_abv_dam_split_baseline = tibble::tibble(
      sim_year               = integer(),
      spawners_abv_dam       = numeric(),
      prop_mccloud     = numeric(),
      prop_little_sac        = numeric(),
      prop_upper_sac_blw     = numeric(),
      spawners_mccloud = numeric(),
      spawners_little_sac    = numeric(),
      spawners_upper_sac_blw = numeric()
    ),
    spawn_prop_us_baseline = tibble::tibble(
      sim_year     = integer(),
      prop_natural = numeric()
    ),
    sub_area_spawners_baseline = tibble::tibble(
      sim_year         = integer(),
      sub_area         = character(),
      spawners         = numeric(),
      prop_natural     = numeric(),
      nat_returns      = numeric(),
      hatchery_returns = numeric(),
      phos             = numeric()
    ),
    baseline_habitat_props = tibble::tibble(
      sim_year           = integer(),
      prop_mccloud = numeric(),
      prop_little_sac    = numeric(),
      prop_upper_sac_blw = numeric()
    ),
    ind_pop_table = tibble::tibble(
      scenario           = character(),
      ind_pop = integer()
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

# by all watersheds
abv_dam_habitat_props <- get_abv_dam_habitat_props(portfolio_param_obj)

spawners_abv_dam_split <- portfolio_res_obj$spawners_abv_dam["Upper Sacramento River", ] |>
  as.data.frame() |>
  setNames("spawners_abv_dam") |>
  mutate(sim_year = 1:20) |>
  left_join(abv_dam_habitat_props) |>
  mutate(
    spawners_mccloud = spawners_abv_dam * prop_mccloud,
    spawners_little_sac    = spawners_abv_dam * prop_little_sac,
    spawners_upper_sac_blw = spawners_abv_dam * prop_upper_sac_blw  # remainder below dam
  )

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
  mutate(scenario = "portfolio") |> 
  bind_rows(baseline_results$proportion_natural_at_spawning[c("Upper Sacramento River", "Battle Creek"),] |>
              as.data.frame() |>
              mutate(watershed = c("Upper Sacramento River", "Battle Creek")) |>
              pivot_longer(`1`:`20`, names_to = "sim_year", values_to = "prop_natural") |> 
              mutate(scenario = "baseline")) |>
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
  left_join(mean_phos) |> 
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
output$abv_dam_habitat_props   <- abv_dam_habitat_props
output$spawners_abv_dam_split  <- spawners_abv_dam_split
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
  # should we add Battle Creek juvs?
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
  filter(watershed %in% c("Upper Sacramento River", "Battle Creek"),
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
  bind_rows(data.frame(sim_year = 1:20,
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
  ungroup() |> 
  group_by(sim_year, scenario) |> 
  summarize(mean_rear_prop = mean(rear_prop))

# rearing prop summary 
rearing_prop_summary <- rearing_prop |> 
  group_by(scenario) |> 
   #TODO maybe add an if_else to have this be 0 when no ASD actions
  reframe(mean_juv_rear_trib = if_else(portfolio_param_obj$abv_dam_spawn_proportion>0,
                                         mean(mean_rear_prop),
                                         0))


### Spawning and rearing habitat above Shasta-------------------
# summary by year
habitat_abv_shasta <- 
  habitat_diff |> 
  group_by(sim_year, scenario) |> 
  mutate(spawn = if_else(hab_diff_spawn >0, 1L, 0L),
         rear = if_else((hab_diff_juv>0 | hab_diff_fp > 0 | hab_diff_fry>0), 1L, 0L),
         habitat_score = sum(spawn, rear) )

mean_habitat_abv_shasta <- habitat_abv_shasta |> 
  group_by(scenario) |> 
  summarize(num_trib = mean(habitat_score)) |> 
  ungroup()


### Independent populations -----------------

# Calculate pHOS for above-dam fish
# proportion_natural_at_spawning applies to Upper Sacramento River as a whole,
# so use it as a proxy — or use proportion_natural_juves_in_tribs if preferred

spawn_prop_us <- portfolio_res_obj$proportion_natural_at_spawning["Upper Sacramento River", ] |>
  as.data.frame() |>
  setNames("prop_natural") |>
  mutate(sim_year = 1:20,
         prop_natural = replace(prop_natural, is.nan(prop_natural), 0))

# Build sub-area spawner table
sub_area_spawners <- spawners_abv_dam_split |>
  select(sim_year, 
         `McCloud`               = spawners_mccloud,
         `Little Sacramento`           = spawners_little_sac,
         `Upper Sacramento (below dam)` = spawners_upper_sac_blw) |>
  bind_rows(
    spawners_split |> 
      filter(watershed == "Battle Creek", scenario == "portfolio") |>
      select(sim_year, `Battle Creek` = spawners)
  ) |>
  pivot_longer(-sim_year, names_to = "sub_area", values_to = "spawners") |>
  left_join(spawn_prop_us |> select(sim_year, prop_natural)) |>
  mutate(
    nat_returns      = spawners * prop_natural,
    hatchery_returns = spawners - nat_returns,
    phos             = hatchery_returns / spawners,
    phos             = replace(phos, is.nan(phos), NA)
  )

# Calculate CRR and growth per sub-area
sub_area_ind_pop <- sub_area_spawners |>
  group_by(sub_area) |>
  mutate(
    crr         = lead(nat_returns, 3) / spawners,
    crr         = replace(crr, is.nan(crr), NA),
    growth_rate = (spawners - lag(spawners)) / lag(spawners),
    decline     = growth_rate,
    cat_decline = (spawners - lag(spawners, 3)) / lag(spawners, 3),
    above_500_spawners       = if_else(spawners > 500, 1L, 0L),
    phos_less_than_5_percent = if_else(phos < 0.05, 1L, 0L),
    crr_above_1              = if_else(crr >= 1, 1L, 0L),
    growth_rate_above_1      = if_else(growth_rate >= 0, 1L, 0L),
    independent_conditions   = if_else(
      above_500_spawners & phos_less_than_5_percent &
        growth_rate_above_1 & crr_above_1, 1L, 0L)
  ) |>
  ungroup() |> 
  mutate(scenario = "portfolio")

# baseline habitat props
baseline_habitat_props <- get_abv_dam_habitat_props(wr_sdm_baseline_params)

# baseline spawners above dam split
spawners_abv_dam_split_baseline <- baseline_results$spawners_abv_dam["Upper Sacramento River", ] |>
  as.data.frame() |>
  setNames("spawners_abv_dam") |>
  mutate(sim_year = 1:20) |>
  left_join(baseline_habitat_props) |>
  mutate(
    spawners_mccloud = spawners_abv_dam * prop_mccloud,
    spawners_little_sac    = spawners_abv_dam * prop_little_sac,
    spawners_upper_sac_blw = spawners_abv_dam * prop_upper_sac_blw
  )

# baseline spawn prop
spawn_prop_us_baseline <- baseline_results$proportion_natural_at_spawning["Upper Sacramento River", ] |>
  as.data.frame() |>
  setNames("prop_natural") |>
  mutate(sim_year = 1:20,
         prop_natural = replace(prop_natural, is.nan(prop_natural), 0))

# baseline sub area spawners
sub_area_spawners_baseline <- spawners_abv_dam_split_baseline |>
  select(sim_year,
         `Upper McCloud`               = spawners_mccloud,
         `Little Sacramento`           = spawners_little_sac,
         `Upper Sacramento (below dam)` = spawners_upper_sac_blw) |>
  bind_rows(
    spawners_split |>
      filter(watershed == "Battle Creek", scenario == "baseline") |>
      select(sim_year, `Battle Creek` = spawners)
  ) |>
  pivot_longer(-sim_year, names_to = "sub_area", values_to = "spawners") |>
  left_join(spawn_prop_us_baseline |> select(sim_year, prop_natural)) |>
  mutate(
    nat_returns      = spawners * prop_natural,
    hatchery_returns = spawners - nat_returns,
    phos             = hatchery_returns / spawners,
    phos             = replace(phos, is.nan(phos), NA)
  )

# baseline ind pop
sub_area_ind_pop_baseline <- sub_area_spawners_baseline |>
  group_by(sub_area) |>
  mutate(
    crr         = lead(nat_returns, 3) / spawners,
    crr         = replace(crr, is.nan(crr), NA),
    growth_rate = (spawners - lag(spawners)) / lag(spawners),
    decline     = growth_rate,
    cat_decline = (spawners - lag(spawners, 3)) / lag(spawners, 3),
    above_500_spawners       = if_else(spawners > 500, 1L, 0L),
    phos_less_than_5_percent = if_else(phos < 0.05, 1L, 0L),
    crr_above_1              = if_else(crr >= 1, 1L, 0L),
    growth_rate_above_1      = if_else(growth_rate >= 0, 1L, 0L),
    independent_conditions   = if_else(
      above_500_spawners & phos_less_than_5_percent &
        growth_rate_above_1 & crr_above_1, 1L, 0L)
  ) |>
  ungroup() |>
  mutate(scenario = "baseline")

# combine baseline and portfolio
sub_area_ind_pop_combined <- bind_rows(
  sub_area_ind_pop_baseline,
  sub_area_ind_pop
)

# last 3 years per sub_area and scenario
sub_area_ind_pop_last3 <- sub_area_ind_pop_combined |>
  filter(!is.na(independent_conditions)) |>
  group_by(sub_area, scenario) |>
  slice_tail(n = 3) |>
  ungroup()

# summary table
ind_pop_table <- sub_area_ind_pop_last3 |>
  group_by(scenario, sub_area) |>
  summarize(
    pct_yrs_independent = mean(independent_conditions, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(is_independent = if_else(pct_yrs_independent == 1, 1L, 0L)) |>
  pivot_wider(names_from = sub_area, values_from = is_independent, id_cols = scenario) |>
  mutate(ind_pop = rowSums(across(-scenario), na.rm = TRUE)) |>
  select(scenario, ind_pop)


### Combine ----------------
populations_table <- mean_spawners_in_tribs |> 
  left_join(rearing_prop_summary) |> 
  left_join(mean_habitat_abv_shasta) |> 
  left_join(ind_pop_table) |> 
  pivot_longer(cols = -scenario,
               names_to = "metric", 
               values_to = "value") |> 
  pivot_wider(names_from = scenario,
              values_from = value) |> 
  mutate(objective = "number of populations")

## Metrics Table ---------------------

metrics_table <- abund_table |>
  bind_rows(prod_table,diversity_table,populations_table) |>
  mutate(watershed = "combined") |>
  select(objective, metric, watershed, baseline, portfolio)|>
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

### output updates ------------------
output$spawners_bc <- spawners_bc
output$spawners_abv_shasta <- spawners_abv_shasta
output$spawners_tribs <- spawners_tribs
output$mean_spawners_in_tribs <- mean_spawners_in_tribs
# output$fry_habitat_abv_shasta <- fry_habitat_abv_shasta
# output$juv_habitat_abv_shasta <- juv_habitat_abv_shasta
# output$fp_habitat_abv_shasta <- fp_habitat_abv_shasta
# output$spawn_habitat_abv_shasta <- spawn_habitat_abv_shasta
output$habitat_diff <- habitat_diff
output$rearing_prop <- rearing_prop
output$rearing_prop_summary <- rearing_prop_summary
output$habitat_abv_shasta <- habitat_abv_shasta
output$mean_habitat_abv_shasta <- mean_habitat_abv_shasta
output$spawn_prop_us                  <- spawn_prop_us
output$sub_area_spawners              <- sub_area_spawners
output$sub_area_ind_pop               <- sub_area_ind_pop
output$baseline_habitat_props         <- baseline_habitat_props
output$spawners_abv_dam_split_baseline <- spawners_abv_dam_split_baseline
output$spawn_prop_us_baseline         <- spawn_prop_us_baseline
output$sub_area_spawners_baseline     <- sub_area_spawners_baseline
output$sub_area_ind_pop_baseline      <- sub_area_ind_pop_baseline
output$sub_area_ind_pop_combined      <- sub_area_ind_pop_combined
output$sub_area_ind_pop_last3         <- sub_area_ind_pop_last3
output$ind_pop_table                  <- ind_pop_table
output$populations_table <- populations_table
output$metrics_table <- metrics_table

return(output)
}


