# Scenarios for WR SDM

# Add x amount of in-channel habitat in Upper Sacramento
wr_sdm_scen1_params <- wr_sdm_baseline_params
wr_sdm_scen1_params$inchannel_habitat_fry["Upper Sacramento River",1:5 ,] = wr_sdm_scen1_params$inchannel_habitat_fry["Upper Sacramento River",1:5 ,]*1.5

# Bump up prey density 
wr_sdm_scen1_params$prey_density["Upper Sacramento River",6:7] = "high"


# Plotting
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
