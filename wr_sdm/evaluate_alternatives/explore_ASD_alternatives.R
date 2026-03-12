library(winterRunDSM)
library(tidyverse)
library(plotly)

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
test_params$adult_enroute_surv_mult["Upper Sacramento River"] <- 2

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
