library(winterRunDSM)
library(tidyverse)
library(plotly)
library(ggplot2)

# baseline
baseline_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                              mode = "seed",
                                              seeds = NULL, 
                                              ..params = winterRunDSM::wr_sdm_baseline_params)

baseline_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                    ..params = winterRunDSM::wr_sdm_baseline_params,
                                                    seeds = baseline_seeds)

# test out adult enroute survival multiplier
test_params <- winterRunDSM::wr_sdm_baseline_params
test_params$above_dam_spawn_proportion <- 0.5
test_params$spawning_habitat["Upper Sacramento River",,] <- DSMhabitat::wr_spawn$action_5_upper_sac_tmh["Upper Sacramento River",,]
test_params$egg_to_fry_survival_abv_dam <- .37
test_params$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean((DSMhabitat::wr_spawn$action_5_upper_sac_tmh["Upper Sacramento River",,] - DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]) / DSMhabitat::wr_spawn$action_5_upper_sac_tmh["Upper Sacramento River",,])
test_params$hatchery_release["Upper Sacramento River","xl",] <- rep(100000, 20)

alt_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                                 mode = "seed",
                                                 seeds = NULL, 
                                                 ..params = test_params)

alt_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                   ..params = test_params,
                                                   seeds = alt_seeds)

baseline_results$spawners |> 
  as.data.frame() |> 
  mutate(watershed = winterRunDSM::watershed_labels,
         scenario = "baseline") |> 
  bind_rows(alt_results$spawners |> 
              as.data.frame() |> 
              mutate(watershed = winterRunDSM::watershed_labels,
                     scenario = "alt")) |> 
  pivot_longer(`1`:`20`,
               names_to = "sim_year",
               values_to = "spawners") |> 
  filter(watershed %in% c("Upper Sacramento River")) |> 
  mutate(sim_year = as.numeric(sim_year)) |> 
  ggplot(aes(x = sim_year,
             y = spawners,
             color = scenario)) +
  geom_line() +
  facet_wrap(~watershed)

baseline_results$total_fry_from_dam |> 
  as.data.frame() |> 
  mutate(watershed = winterRunDSM::watershed_labels,
         scenario = "baseline") |> 
  bind_rows(alt_results$total_fry_from_dam |>
              as.data.frame() |>
              mutate(watershed = winterRunDSM::watershed_labels,
                     scenario = "alt")) |>
  pivot_longer(`1`:`20`,
               names_to = "sim_year",
               values_to = "fry") |> 
  filter(watershed %in% c("Upper Sacramento River")) |> 
  mutate(sim_year = as.numeric(sim_year)) |> 
  ggplot(aes(x = sim_year,
             y = fry,
             color = scenario)) +
  geom_line() +
  facet_wrap(~watershed)




baseline_results$juveniles_at_chipps |> 
  as.data.frame() |> 
  mutate(scenario = "baseline") |> 
  bind_rows(alt_results$juveniles_at_chipps |> 
              as.data.frame() |> 
              mutate(scenario = "alt")) |> 
  filter(watershed %in% c("Upper Sacramento River")) |> 
  mutate(year = as.numeric(year)) |> 
  ggplot(aes(x = factor(year),
             y = juveniles_at_chipps,
             color = scenario)) +
  geom_boxplot()
  # facet_wrap(~watershed)



jac <- alt_results$juveniles_at_chipps
jac |> filter(juveniles_at_chipps!=0, grepl("Sacramento", watershed)) |> 
  ggplot() + 
  geom_col(aes(year, juveniles_at_chipps, fill = factor(month))) 
