# Scenario example for Winter-Run SDM
# Create scenario and plot the model output

# Create scenarios --------------

## Add x amount of in-channel habitat in Upper Sacramento ------------
wr_sdm_scen1_params <- wr_sdm_baseline_params
wr_sdm_scen1_params$inchannel_habitat_fry["Upper Sacramento River", ,] = wr_sdm_scen1_params$inchannel_habitat_fry["Upper Sacramento River",,]*20
wr_sdm_scen1_params$inchannel_habitat_juvenile["Upper Sacramento River", ,] = wr_sdm_scen1_params$inchannel_habitat_juvenile["Upper Sacramento River",,]*20
wr_sdm_scen1_params$spawning_habitat["Upper Sacramento River",,] = wr_sdm_scen1_params$spawning_habitat["Upper Sacramento River",,]*20
wr_sdm_scen1_params$spawning_habitat["Battle Creek",,] = wr_sdm_scen1_params$spawning_habitat["Battle Creek",,]*10

## Hatchery action ----------------------------------
wr_sdm_scen1_params$hatchery_release["Upper Sacramento River", "xl",] <- 20000

## Bump up prey density ------------------------
wr_sdm_scen1_params$prey_density["Upper Sacramento River",] = "max"
wr_sdm_baseline_params$prey_density

### Plotting to compare habitat inputs-----------------
compare_habitat_inputs <- wr_sdm_baseline_params$inchannel_habitat_fry["Upper Sacramento River",,] |> 
  as.data.frame() |> 
  mutate(month = month.abb) |> 
  pivot_longer(cols = `1980`:`2000`,
               names_to = "year",
               values_to = "ic_habitat") |> 
  mutate(scenario = "baseline") |> 
  bind_rows(wr_sdm_scen1_params$inchannel_habitat_fry["Upper Sacramento River",,] |> 
              as.data.frame() |> 
              mutate(month = month.abb) |> 
              pivot_longer(cols = `1980`:`2000`,
                           names_to = "year",
                           values_to = "ic_habitat") |> 
              mutate(scenario = "scen1")) |> 
            mutate( date = as.Date(paste0(year, "-", month, "-01"), 
                           format = "%Y-%b-%d"))

compare_habitat_inputs |> 
  ggplot(aes(x = date, y = ic_habitat, color = scenario)) + 
  geom_line()

# Compare baseline and scenario models  ------------
## baseline -----------------
wr_sdm_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                               mode = "seed",
                                               seeds = NULL, 
                                               ..params = winterRunDSM::wr_sdm_baseline_params)

wr_sdm_model_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                       ..params = winterRunDSM::wr_sdm_baseline_params,
                                                       seeds = wr_sdm_seeds)

spawn <- dplyr::as_tibble(wr_sdm_model_results$spawners) |>
  dplyr::mutate(location = winterRunDSM::watershed_labels) |>
  pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") |> 
  filter(location == "Upper Sacramento River")|> 
  mutate(scen = "baseline")


## scenario1 ----------------
wr_sdm_model_scen1_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                       ..params = wr_sdm_scen1_params,
                                                       seeds = wr_sdm_seeds)

spawn_scen1 <- dplyr::as_tibble(wr_sdm_model_scen1_results$spawners) |>
  dplyr::mutate(location = winterRunDSM::watershed_labels) |>
  pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") |> 
  filter(location == "Upper Sacramento River") |> 
  mutate(scen = "scen1")

# both data frames
spawn_all <- bind_rows(spawn, spawn_scen1) %>%
  mutate(year= as.numeric(year))

### Plot ---------------
ggplot() +
    geom_col(data = spawn_all, aes(year, spawners, fill
                                   = scen), position = position_dodge2())
