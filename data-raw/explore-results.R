library(winterRunDSM)
library(tidyverse)
library(plotly)
library(wr_sdmscenario)

wr_sdm_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                              mode = "seed",
                                              seeds = NULL, 
                                              ..params = winterRunDSM::wr_sdm_baseline_params)

wr_sdm_model_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                    ..params = winterRunDSM::wr_sdm_baseline_params,
                                                    seeds = wr_sdm_seeds)
wr_sdm_model_results$spawners
wr_sdm_model_results$phos

# TODO any others?
spawn_regions <- c("Upper Sacramento River", "Battle Creek")

spawn <- dplyr::as_tibble(wr_sdm_model_results$spawners) |>
  dplyr::mutate(location = winterRunDSM::watershed_labels) |>
  pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") |>
  dplyr::filter(location %in% spawn_regions) |>
  group_by(year, location) |>
  summarize(total_spawners = sum(spawners)) |>
  filter(location != "Feather River") |>
  mutate(year = as.numeric(year)) |>
  ggplot(aes(year, total_spawners, color = location)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 20))

ggplotly(spawn)

# CHECK against grandtab
grandtab_totals <- dplyr::as_tibble(DSMCalibrationData::grandtab_observed$winter)|> 
  dplyr::mutate(location = winterRunDSM::watershed_labels) |>
  pivot_longer(cols = c(`1998`:`2017`), values_to = 'spawners', names_to = "year") |> 
  dplyr::filter(location %in% spawn_regions) |>
  group_by(year,
           location
  ) |>
  # group_by(year) |>
  summarize(total_spawners = sum(spawners, na.rm = TRUE)) |>
  mutate(year = as.numeric(year)) %>%
  # ggplot(aes(x = year, y= total_spawners)) +
  ggplot(aes(year, total_spawners,
             color = location
  )) +
  geom_line() +
  theme_minimal() +
  labs(y = "Grand tab Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 20))

ggplotly(grandtab_totals)
