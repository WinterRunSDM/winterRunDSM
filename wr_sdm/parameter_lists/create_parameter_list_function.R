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
  
  # Data prep
  upper_sac_w_mccloud <- DSMhabitat::wr_fry$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,]
  upper_sac_wo_mccloud <- DSMhabitat::wr_fry$action_5["Upper Sacramento River",,]
  mccloud_addition <- upper_sac_w_mccloud - upper_sac_wo_mccloud
  upper_mccloud_addition <- mccloud_addition * 0.3
  lower_mccloud_addition <- mccloud_addition * 0.7
  # ds3_inchannel_hab <- DSMhabitat::wr_fry$action_5["Upper Sacramento River",,] + upper_mccloud_addition
  # ds4_inchannel_hab <- DSMhabitat::wr_fry$action_5["Upper Sacramento River",,] + lower_mccloud_addition
  
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
    # TODO do we need to do anything with temperature suitability here? 
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,]*2
    param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,]*2
  }
  
  if(action_id == "SR-2c") {
    # TODO add rearing habitat
    # TODO move up size class
    }
  
  if(action_id == "SR-3") {
    # TODO add spawning habitat; adjust for temperature
  }
  
  if(action_id == "SR-4a") {
    # reduce predator contact points by 25%
    param_list$contact_points <- round(param_list$contact_points * 0.75)
  }
  
  if(action_id == "SR-4b") {
    # reduce prop high predation
    # TODO compare with 4a and make a decision
    param_list$prop_high_predation["Upper Sacramento River"] <- 0.2 
  }
  
  if(action_id == "SR-5") {
    # reduce effect by 75% - we saw some effect in the spawners but not too drastic
    param_list$.surv_juv_rear_prop_diversions <- param_list$.surv_juv_rear_prop_diversions * 0.75
  }
  
  if(action_id == "SR_6"){
    # Change routing for Georgiana Slough
    # TODO decide on parameter
  }
  
  # TODO waiting on response from Sarah - likely to not model
  if(action_id == "SR_7"){
    # DCC gates 
  }
  
  if(action_id == "SR-8a") {
    # TODO TBD on the reduction percentage
    param_list$contact_points <- round(param_list$contact_points * 0.5)
  }
  
  if(action_id == "SR-8b") {
    # reduce prop high predation
    param_list$prop_high_predation["Upper Sacramento River"] <- 0.2 
  }
 
  if(action_id == "SR-9") {
    # TODO implement this change in model script? or spawn_success? 
    param_list$effect_upstream_vol_adult_kwk <- 0.99
    param_list$effect_upstream_vol_juv_kwk <- 0.97
    # TODO modify this value
    param_list$spawning_habitat["Upper-mid Sacramento River",,] <- param_list$spawning_habitat["Upper-mid Sacramento River",,] + 4
  }
  
  
  if(action_id == "SR-11") {
    param_list$addl_juv_chipps <- 50000
  }
  
  # TODO wait on Rene
  if(action_id == "SR-12") {
  }
  
  # Above Shasta ----------------
  # Battle Creek--------
  
  # BC-1
  # Right now we are only including ocean harvest
  if(action_id == "BC-1") {
    # change the incidental/illegal harvest rate
    param_list$harvest_rate_trib["Battle Creek"] <- param_list$harvest_rate_trib["Battle Creek"] * 0.5
  }
  
  # BC-3 
  # No model inputs
  
  
  # BC-8
  if(action_id == "BC-8") {
    param_list$hatchery_release["Battle Creek","l",] <- rep(200000, 20)
  }
 
  
   # TODO modify model code based on a threshold of adults returning to Battle Creek
  # BC-9
  if(action_id == "BC-9") {
    param_list$hatchery_release["Battle Creek","l",] <- rep(200000, 20)
    
    param_list$natural_adult_removal_rate["Battle Creek"] <- 0.25
  }
  
  # Other ---------------------
  # TODO figure out if someone really wants this action
  # Why are salvaged fish trucked to the South Delta and not Chipps? 
  # O-1 
  
  
  # O-2
  if(action_id == "O-2") {
  # change the incidental/illegal harvest rate
    param_list$harvest_rate_ocean <- param_list$harvest_rate_ocean * 0.99
    }
  
  # O-3 
  # percent of age class harvested 
  # if(action_id == "O-3"){
  #   
  # }
  
  # ASD ------------------
  
  # ASD-1
  if(action_id == "ASD-1") {
    param_list$hatchery_release["Upper Sacramento River","m",] <- rep(300000, 20)
    param_list$hatchery_release["Upper Sacramento River","l",] <- rep(200000, 20)
    
    #TODO add egg-to-fry survival 
  }
  
  # ASD-2
  if(action_id == "ASD-2") {
    param_list$harvest_rate_abv_dam <- param_list$harvest_rate_abv_dam * 0.5
  }
  
  # ASD-3
  if(action_id == "ASD-3") {
    # TODO may change hatchery release to reflect a portion of fish emigrating as smolts due to yearling behavior
    # TODO figure out how to shift juvenile rearing size class distribution
    # TODO add egg to fry survival
    param_list$hatchery_release["Upper Sacramento River","s",] <- rep(c(800000, 10), c(150000, 10))
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + upper_mccloud_addition
    param_list$juvenile_capture_efficiency <- 0.25
    param_list$effect_downstream_trap_juvenile_abv_dam <- 0.8
  }
  
  # ASD-4
  if(action_id == "ASD-4") {
    # TODO may change hatchery release to reflect a portion of fish emigrating as smolts due to yearling behavior
    # TODO figure out how to shift juvenile rearing size class distribution
    # TODO add egg to fry survival
    param_list$hatchery_release["Upper Sacramento River","s",] <- rep(c(800000, 10), c(150000, 10))
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + lower_mccloud_addition
    param_list$juvenile_capture_efficiency <- 0.25
    param_list$effect_downstream_trap_juvenile_abv_dam <- 0.8
  }

  
  # Facilities -------
  
  # Other -----------
  
}
