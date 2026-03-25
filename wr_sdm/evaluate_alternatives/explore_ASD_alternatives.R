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
alt_params <- create_param_list(action_ids = c("BC-5", "BC-8"))

alt_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                                 mode = "seed",
                                                 seeds = NULL, 
                                                 ..params = winterRunDSM::wr_sdm_baseline_params)

alt_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                   ..params = winterRunDSM::wr_sdm_baseline_params,
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
  filter(watershed %in% c("Upper Sacramento River", "Battle Creek")) |> 
  mutate(sim_year = as.numeric(sim_year)) |> 
  ggplot(aes(x = sim_year,
             y = spawners,
             color = scenario)) +
  geom_line() +
  facet_wrap(~watershed, scales = "free_y")

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
  filter(watershed %in% c("Upper Sacramento River", "Battle Creek")) |> 
  mutate(year = as.numeric(year)) |> 
  ggplot(aes(x = factor(year),
             y = juveniles_at_chipps,
             color = scenario)) +
  geom_boxplot()
  # facet_wrap(~watershed)

baseline_results$rearing_survival_inchannel |> 
  as.data.frame.table(responseName = "value") |> 
  rename(watershed = Var1, size_class = Var2, year = Var3,
         rear_surv_fp = value) |> 
  mutate(scenario = "baseline") |> 
  bind_rows(alt_results$rearing_survival_inchannel |> 
              as.data.frame.table(responseName = "value") |> 
              rename(watershed = Var1, size_class = Var2, year = Var3,
                     rear_surv_fp = value) |> 
              mutate(scenario = "alt")) |> 
  mutate(year = as.integer(year)) |> 
  filter(watershed == "Upper Sacramento River") |> 
  ggplot(aes(x = year,
             y = rear_surv_fp,
             color = scenario)) +
  geom_line() +
  facet_wrap(~size_class)



jac <- baseline_results$juveniles_at_chipps
jac |> filter(juveniles_at_chipps!=0, grepl("Sacramento", watershed)) |> 
  ggplot() + 
  geom_col(aes(year, juveniles_at_chipps, fill = factor(month))) 


# outmigration
baseline_results$upper_mid_sac_fish |> 
  as.data.frame.table(responseName = "value") |> 
  rename(month = Var1, size_class = Var2, year = Var3,
         up_sac_fish = value) |> 
  mutate(scenario = "baseline") |> 
  bind_rows(alt_results$upper_mid_sac_fish |>
              as.data.frame.table(responseName = "value") |>
              rename(month = Var1, size_class = Var2, year = Var3,
                     up_sac_fish = value) |>
              mutate(scenario = "alt")) |>
  mutate(year = as.character(year),
         month = factor(month, levels = as.character(c(9:12, 1:5)))) |> 
  group_by(year, month, scenario) |> 
  summarise(total_up_sac_fish = sum(up_sac_fish, na.rm = T)) |> 
  ungroup() |>
  ggplot(aes(x = month,
             y = total_up_sac_fish,
             color = year,
             group = year)) +
  geom_line() +
  facet_wrap(~scenario)

baseline_results$lower_mid_sac_fish |> 
  as.data.frame.table(responseName = "value") |> 
  rename(month = Var1, size_class = Var2, year = Var3,
         lower_mid_sac_fish = value) |> 
  mutate(scenario = "baseline") |> 
  bind_rows(alt_results$lower_mid_sac_fish |>
              as.data.frame.table(responseName = "value") |>
              rename(month = Var1, size_class = Var2, year = Var3,
                     lower_mid_sac_fish = value) |>
              mutate(scenario = "alt")) |>
  mutate(year = as.character(year),
         month = factor(month, levels = as.character(c(9:12, 1:5)))) |> 
  group_by(year, month, scenario) |> 
  summarise(total_lower_mid_sac_fish = sum(lower_mid_sac_fish, na.rm = T)) |> 
  ungroup() |>
  ggplot(aes(x = month,
             y = total_lower_mid_sac_fish,
             color = year,
             group = year)) +
  geom_line() +
  facet_wrap(~scenario)

baseline_results$lower_sac_fish |> 
  as.data.frame.table(responseName = "value") |> 
  rename(month = Var1, size_class = Var2, year = Var3,
         lower_sac_fish = value) |> 
  mutate(scenario = "baseline") |> 
  bind_rows(alt_results$lower_sac_fish |>
              as.data.frame.table(responseName = "value") |>
              rename(month = Var1, size_class = Var2, year = Var3,
                     lower_sac_fish = value) |>
              mutate(scenario = "alt")) |>
  mutate(year = as.character(year),
         month = factor(month, levels = as.character(c(9:12, 1:5)))) |> 
  group_by(year, month, scenario) |> 
  summarise(total_lower_sac_fish = sum(lower_sac_fish, na.rm = T)) |> 
  ungroup() |>
  ggplot(aes(x = month,
             y = total_lower_sac_fish,
             color = year,
             group = year)) +
  geom_line() +
  facet_wrap(~scenario)


outmigrating_sac_fish <- baseline_results$lower_sac_fish |> 
  as.data.frame.table(responseName = "value") |> 
  rename(month = Var1, size_class = Var2, year = Var3,
         lower_sac_fish = value) |> 
  left_join(baseline_results$lower_mid_sac_fish |> 
              as.data.frame.table(responseName = "value") |> 
              rename(month = Var1, size_class = Var2, year = Var3,
                     lower_mid_sac_fish = value)) |> 
  left_join(baseline_results$upper_mid_sac_fish |> 
              as.data.frame.table(responseName = "value") |> 
              rename(month = Var1, size_class = Var2, year = Var3,
                     upper_mid_sac_fish = value)) |> 
  mutate(lower_sac_fish = ifelse(is.nan(lower_sac_fish), 0, lower_sac_fish),
         upper_mid_sac_fish = ifelse(is.nan(upper_mid_sac_fish), 0, upper_mid_sac_fish),
         lower_mid_sac_fish = ifelse(is.nan(lower_mid_sac_fish), 0, lower_mid_sac_fish)) |> 
  mutate(total_fish = lower_sac_fish + lower_mid_sac_fish + upper_mid_sac_fish) |> 
  glimpse()

all_outmigrating_sac_fish <- outmigrating_sac_fish |> 
  # sum
  group_by(year, month) |> 
  summarise(total = sum(total_fish)) |> 
  ungroup() |> 
  glimpse()

all_outmigrating_sac_fish |> 
  # plot
  ggplot(aes(x = month, y = total, color = year, group = year)) +
  geom_line() +
  theme_minimal() +
  # labels
  labs(x = "Migration month",
       y = "Juveniles in Sac",
       title = "Sacramento juveniles",
       subtitle = "(Summed across upper-mid, lower-mid, and lower sac)")

outmigrating_sac_fish |> 
  select(-total_fish) |> 
  pivot_longer(lower_sac_fish:upper_mid_sac_fish,
               names_to = "source",
               values_to = "total") |> 
  group_by(year, month, source) |> 
  summarise(total = sum(total)) |> 
  ungroup() |> 
  # plot
  ggplot(aes(x = month, y = total, color = year, group = year)) +
  geom_line() +
  theme_minimal() +
  geom_hline(aes(yintercept = 5000)) +
  # labels
  labs(x = "Migration month",
       y = "Juveniles in Sac",
       title = "Sacramento juveniles",
       subtitle = "(Summed across upper-mid, lower-mid, and lower sac)") +
  facet_wrap(~source, nrow = 3, scales = "free_y")

all_outmigrating_sac_fish |> 
  ggplot() + 
  geom_col(aes(year, total, fill = factor(month)), position = "fill") +
  theme_minimal() +
  labs(x = "Sim year",
       y = "Proportion of juveniles in Sac by month",
       title = "Sacramento juveniles",
       subtitle = "(Summed across upper-mid, lower-mid, and lower sac)")

# old code
action_params <- readr::read_csv("wr_sdm/documentation/WRCS_MASTER_Actions_2026-03-10.csv") |>
  janitor::clean_names() |>
  dplyr::select(-short_description) |>
  tidyr::pivot_longer(hatchery_releases:effect_of_downstream_volitional_passage_on_juvenile_salmon,
               names_to = "parameter_title",
               values_to = "action_requires") |>
  dplyr::filter(!is.na(action_requires),
         !is.na(action_id)) |>
  dplyr::arrange(parameter_title)

impossible_combinations <- c()

  
