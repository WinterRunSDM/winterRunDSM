library(winterRunDSM)
library(tidyverse)
library(plotly)
library(ggplot2)
source(here::here("wr_sdm", "parameter_lists", "create_parameter_list_function.R"))

# baseline
baseline_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                              mode = "seed",
                                              seeds = NULL, 
                                              ..params = winterRunDSM::wr_sdm_baseline_params)

baseline_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                    ..params = winterRunDSM::wr_sdm_baseline_params,
                                                    seeds = baseline_seeds)

# test out adult enroute survival multiplier
alt_params <- create_param_list(action_ids = c("SR-3"))

alt_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                                 mode = "seed",
                                                 seeds = NULL, 
                                                 ..params = alt_params)

alt_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                   ..params = alt_params,
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
