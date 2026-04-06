# Script to pair with summarize_outputs_scen_intro.qmd. 
# Adapt to have this work for all portfolios, or model others after this. 
# Creates table of all the performance metrics for portfolio. 
library(zoo)
library(dplyr)
library(tidyr)
library(purrr)
library(winterRunDSM)

source("wr_sdm/portfolios/calculate_habitat_prop_portfolios.R")

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
    
    
    # JUVENILES - UPPER SAC
    juvs_us = tibble::tibble(
      watershed = character(),
      scenario  = character(),
      year      = integer(),
      total_juv = numeric()
    ),
    
    # JUVENILES AT CHIPPS
    juv_chipps = tibble::tibble(
      scenario = character(),
      year     = integer(),
      jac      = numeric()
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
    

    # POPULATIONS IN TRIBS - Spawners
    # spawners_abv_shasta = tibble::tibble(
    #   watershed = character(),
    #   sim_year  = integer(),
    #   spawners  = numeric(),
    #   scenario  = character()
    # ),
    spawners_tribs = tibble::tibble(
      scenario  = character(),
      sim_year  = integer(),
      spawners  = numeric()
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
    hab_prop = tibble::tibble(
    portfolio = character(),
    sim_year     = integer(),
     prop_bc_rear = numeric(),
    prop_mccloud_rear = numeric(),
    prop_little_sac_rear = numeric(),
    prop_asd_rear = numeric(),
    prop_trib_rear = numeric(),
    prop_mccloud_spawn = numeric(),
    prop_little_sac_spawn = numeric(),
    prop_asd_spawn = numeric(),
    prop_trib_spawn = numeric(),
     scenario = character()
    ),

    habitat_abv_shasta = tibble::tibble(
      sim_year        = integer(),
      scenario        = character(),
     rear_mccloud  = numeric(),
      rear_littlesac  = numeric(),
      spawn_mccloud    = numeric(),
      spawn_littlesac    = numeric(),
      mccloud_score     = numeric(),
      littlesac_score     = numeric(),
      habitat_score   = integer()
    ),
    
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

    ind_pop_table = tibble::tibble(
      scenario           = character(),
      ind_pop = integer()
    ),
    
    metrics_table_raw = tibble::tibble(
      objective = character(),
      metric    = character(),
      watershed = character(),
      baseline = numeric(),
      portfolio = numeric(),
      `Baseline Results`  = character(),
      `Portfolio Results` = character()
    ),

    metrics_table = tibble::tibble(
      Objective = character(),
      Metric    = character(),
      Watershed = character(),
      watershed = character(),
      `Baseline Results`  = character(),
      `Portfolio Results` = character()
      
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
  mutate(sim_year = as.integer(1:20)) |>
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
output$abv_dam_habitat_props   <- abv_dam_habitat_props
output$spawners_abv_dam_split  <- spawners_abv_dam_split
output$spawn_prop <- spawn_prop
output$returns_split <- returns_split
output$returns <- returns
output$decline <- decline
output$max_cat_declines <- max_cat_declines

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
output$juv_chipps <- juv_chipps
output$crr <- crr
output$crr_split <- crr_split
output$crr_summary <- crr_summary

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
  left_join(mean_phos |> rename(mean_phos_d = mean_phos)) |> 
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

### Rearing habitat proportion -----------------

# use the portfolio selected to get the rearing habitat proportion 
portfolio_obj <- deparse(substitute(portfolio_res_obj))
portfolio_num <- stringr::str_extract(portfolio_obj, "p\\d+")

# combine
trib_hab_all <- bind_rows(trib_hab_baseline, trib_hab_portfolios)

hab_prop <- trib_hab_all |> 
  filter(portfolio == portfolio_num) 

rearing_prop_summary  <- hab_prop |> 
  group_by(scenario) |> 
  summarize(mean_rear_prop = mean(prop_trib_rear))


### spawning and rearing abv shasta -----------
habitat_abv_shasta <- hab_prop |> 
  group_by(sim_year, scenario) |> 
  summarize(rear_mccloud = if_else(prop_mccloud_rear>0, 1L, 0L),
         rear_littlesac = if_else(prop_little_sac_rear>0, 1L, 0L),
         spawn_mccloud = if_else(prop_mccloud_spawn>0, 1L, 0L),
         spawn_littlesac = if_else(prop_little_sac_spawn>0, 1L, 0L),
         mccloud_score = if_else(sum(rear_mccloud, spawn_mccloud)==2L, 1L, 0L),
         littlesac_score = if_else(sum(rear_littlesac, spawn_littlesac)==2L, 1L, 0L),
         habitat_score = littlesac_score + mccloud_score) |> 
  ungroup()

mean_habitat_abv_shasta <- habitat_abv_shasta |> 
  group_by(scenario) |> 
  summarize(hab_access = mean(habitat_score)) |> 
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

sub_area_spawners <- spawners_abv_dam_split |>
  select(sim_year,
         `McCloud`                      = spawners_mccloud,
         `Little Sacramento`            = spawners_little_sac,
         `Upper Sacramento (below dam)` = spawners_upper_sac_blw) |>
  left_join(
    spawners_split |>
      mutate(sim_year = as.numeric(sim_year)) |>
      filter(watershed == "Battle Creek", scenario == "portfolio") |>
      select(sim_year, `Battle Creek` = spawners),
    by = "sim_year"
  ) |>
  pivot_longer(-sim_year, names_to = "sub_area", values_to = "spawners") |>
  left_join(spawn_prop_us |> select(sim_year, prop_natural), by = "sim_year") |>
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
  left_join(
    spawners_split |>
      mutate(sim_year = as.numeric(sim_year)) |>
      filter(watershed == "Battle Creek", scenario == "baseline") |>
      select(sim_year, `Battle Creek` = spawners),
    by = "sim_year"
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
metrics_table_raw <- abund_table |>
  bind_rows(prod_table,diversity_table,populations_table) |>
  mutate(watershed = "combined") |>
  select(objective, metric, watershed, baseline, portfolio)|>
  mutate(objective = factor(objective, levels = c("abundance", "productivity", "diversity and fitness", "number of populations"))) |>
  arrange(objective) |>  
  left_join(obj_metrics) |>
  mutate(`Baseline Results` = format_metric(baseline),
         `Portfolio Results` = format_metric(portfolio))

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
output$spawners_tribs <- spawners_tribs
output$hab_prop <- hab_prop
output$habitat_abv_shasta <- habitat_abv_shasta
output$spawn_prop_us                  <- spawn_prop_us
output$sub_area_ind_pop_combined      <- sub_area_ind_pop_combined
output$sub_area_ind_pop_last3         <- sub_area_ind_pop_last3
output$metrics_table <- metrics_table
output$metrics_table_raw <- metrics_table_raw

return(output)
}


