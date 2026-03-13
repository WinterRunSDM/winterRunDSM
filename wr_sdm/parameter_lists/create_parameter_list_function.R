# work through approaches


action_params <- read_csv("wr_sdm/documentation/WRCS_MASTER_Actions_2026-03-10.csv") |> 
  janitor::clean_names() |> 
  select(-short_description) |> 
  pivot_longer(hatchery_releases:effect_of_downstream_volitional_passage_on_juvenile_salmon,
               names_to = "parameter_title",
               values_to = "action_requires") |> 
  filter(!is.na(action_requires),
         !is.na(action_id)) |> 
  arrange(parameter_title)

action_params |> filter(action_id == "SR-3")



create_param_list <- function(action_id) {
  param_list <- wr_sdm_baseline_params
  
  #Hatchery -----
  
  # H1
  if(action_id == "H-1") {
    # change hatchery_release
    param_list$hatchery_release["Upper Sacramento River","l",] <- rep(280000, 20)
    param_list$hatchery_release["Upper-mid Sacramento River","l",] <- rep(90000, 20)
    param_list$hatchery_release["Lower-mid Sacramento River","l",] <- rep(90000, 20)
    param_list$hatchery_release["Lower Sacramento River","l",] <- rep(90000, 20)
  }
  
  # H-2b
  if(action_id == "H-2b") {
    # change adult removal rate
    param_list$natural_adult_removal_rate["Upper Sacramento River"] <- 0.15
  }
  
  # H-2c
  if(action_id == "H-2c") {
    # change adult removal rate
    param_list$natural_adult_removal_rate["Upper Sacramento River"] <- 0.25
  }
  
  # H3
  if(action_id == "H-3") {
    # through explorations, this has a significant effect on adult returns. need to be cautious
    param_list$adult_enroute_surv_mult["Upper Sacramento River"] <- param_list$adult_enroute_surv_mult["Upper Sacramento River"] * 1.1
  }
  
  # Sacramento River ----------------
  if(action_id == "SR-1") {
    # triple floodplain habitat in Upper-mid Sacramento River
    param_list$floodplain_habitat["Upper-mid Sacramento River",,] <- param_list$floodplain_habitat["Upper-mid Sacramento River",,]*3
  }
  
  if(action_id == "SR-2a") {
    # double instream habitat in Upper-mid Sacramento River
    # TODO do we need to do anything with temperature suitability here? 
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,]*2
    param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,]*2
  }
  
  if(action_id == "SR-2b") {
    # double instream habitat in Upper-mid Sacramento River
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,]*2
    param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,]*2
  }
  
  if(action_id == "SR-4a") {
    # reduce predator contact points by 25%
    param_list$contact_points <- round(param_list$contact_points * 0.75)
  }
  
  if(action_id == "SR-4b") {
    # reduce prop high predation
    param_list$prop_high_predation["Upper Sacramento River"] <- 0.2 
  }
  
  if(action_id == "SR-5") {
    # reduce effect by 75%
    param_list$.surv_juv_rear_prop_diversions <- param_list$.surv_juv_rear_prop_diversions * 0.75
  }
  
  if(action_id == "SR-8a") {
    # TODO TBD on the reduction percentage
    param_list$contact_points <- round(param_list$contact_points * 0.5)
  }
  
  if(action_id == "SR-8b") {
    # reduce prop high predation
    param_list$prop_high_predation["Upper Sacramento River"] <- 0.2 
  }
  # Above Shasta ----------------
  if(action_id == "SR-9") {
    # TODO implement this change in model script? or spawn_success? 
    param_list$effect_upstream_vol_adult_kwk <- 0.99
    param_list$effect_upstream_vol_juv_kwk <- 0.97
    # TODO modify this value
    param_list$spawning_habitat["Upper-mid Sacramento River",,] <- param_list$spawning_habitat["Upper-mid Sacramento River",,] + 4
  }
  
  
  # Battle Creek--------
  
  # BC-1
  # Right now we are only including ocean harvest
  if(action_id == "BC-1") {
    # change the incidental/illegal harvest rate
    param_list$harvest_rate_trib["Battle Creek"] <- 0.05
  }
  
  # BC-8
  if(action_id == "BC-8") {
    param_list$hatchery_release["Battle Creek","l",] <- rep(200000, 20)
    #TODO modify stray rate (scale down by 10%)
  }
  # BC-9
  if(action_id == "BC-9") {
    param_list$hatchery_release["Battle Creek","l",] <- rep(200000, 20)
    # TODO modify this based on a threshold of adults returning to Battle Creek
    param_list$natural_adult_removal_rate["Battle Creek"] <- 0.25
    #TODO modify stray rate (scale down by 10%)
  }
  
  # Other ---------------------
  # O-2
  if(action_id == "O-2") {
  # change the incidental/illegal harvest rate
    param_list$harvest_rate_ocean <- param_list$harvest_rate_ocean * 0.99
    }
  
  
  
  
  # Facilities -------
  
  # Other -----------
  
}
