#' @title Create parameter list for portfolios
#' @description Creates a new parameter list for input to the winter run model that uses
  #' `winterRunDSM::wr_sdm_baseline_params` as a basis.
#' @param action_ids A vector of action IDs to represent in the parameters. 
#' @export
create_param_list <- function(action_ids) {
  param_list <- winterRunDSM::wr_sdm_baseline_params
  

  # temperature scaling factors
  # calculate habitat additions - too much code here so i moved it to a subfunction
  habitat_additions <- calculate_habitat_additions_ASD_BC()
  
  #Hatchery -----
  
  # H1
  if("H-1" %in% action_ids) {
    # change hatchery_release
    param_list$hatchery_release["Upper Sacramento River","l",] <- param_list$hatchery_release["Upper Sacramento River","l",] + 30000
    param_list$hatchery_release["Upper-mid Sacramento River","l",] <- param_list$hatchery_release["Upper-mid Sacramento River","l",] + 90000
    param_list$hatchery_release["Lower-mid Sacramento River","l",] <- param_list$hatchery_release["Lower-mid Sacramento River","l",] + 90000
    param_list$hatchery_release["Lower Sacramento River","l",] <- param_list$hatchery_release["Lower Sacramento River","l",] + 90000
  }
  
  # H-2b
  if("H-2b" %in% action_ids) {
    # change adult removal rate
    param_list$natural_adult_removal_rate["Upper Sacramento River"] <- 0.15
  }
  
  # H-2c
  if("H-2c" %in% action_ids) {
    # change adult removal rate
    # will default to H-2c rate (not additive with H-2b)
    param_list$natural_adult_removal_rate["Upper Sacramento River"] <- 0.25
  }
  
  # H3
  if("H-3" %in% action_ids) {
    # through explorations, this has a significant effect on adult returns. need to be cautious
    param_list$adult_enroute_surv_mult["Battle Creek"] <- param_list$adult_enroute_surv_mult["Battle Creek"] * 1.1
  
  }
  
  # Sacramento River ----------------
  if("SR-1" %in% action_ids) {
    # triple floodplain habitat in Upper-mid Sacramento River
    param_list$floodplain_habitat["Upper-mid Sacramento River",,] <- param_list$floodplain_habitat["Upper-mid Sacramento River",,]*3
    # param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,]*3
    # param_list$prey_density["Upper-mid Sacramento River",] <- "low"
  }
  
  if("SR-2a" %in% action_ids) {
    # double instream habitat in Upper-mid Sacramento River
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] + wr_sdm_baseline_params$inchannel_habitat_juvenile["Upper Sacramento River",,]
    param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,] + wr_sdm_baseline_params$inchannel_habitat_juvenile["Upper-mid Sacramento River",,]
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + wr_sdm_baseline_params$inchannel_habitat_fry["Upper Sacramento River",,]
    param_list$inchannel_habitat_fry["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper-mid Sacramento River",,]+ wr_sdm_baseline_params$inchannel_habitat_juvenile["Upper-mid Sacramento River",,]
  }
  
  if("SR-2b" %in% action_ids) {
    # double instream habitat in Upper-mid Sacramento River
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] + wr_sdm_baseline_params$inchannel_habitat_juvenile["Upper Sacramento River",,]
    param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,] + wr_sdm_baseline_params$inchannel_habitat_juvenile["Upper-mid Sacramento River",,]
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + wr_sdm_baseline_params$inchannel_habitat_fry["Upper Sacramento River",,]
    param_list$inchannel_habitat_fry["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper-mid Sacramento River",,]+ wr_sdm_baseline_params$inchannel_habitat_juvenile["Upper-mid Sacramento River",,]
  }
  
  if("SR-2c" %in% action_ids) {
    param_list$non_natal_proportion_shift <- 0.6 # could be 45% - 60%
  }
  
  if("SR-3" %in% action_ids) {
    param_list$egg_to_fry_survival_mult["Upper Sacramento River"] <- 1.1
  }
  
  if("SR-4a" %in% action_ids) {
    # reduce predator contact points by 25%
    param_list$contact_points["Upper Sacramento River"] <- round(param_list$contact_points["Upper Sacramento River"] * 1.25) # calibration issue 0.75)
    param_list$contact_points["Upper-mid Sacramento River"] <- round(param_list$contact_points["Upper-mid Sacramento River"] * 1.25) # calibration issue 0.75)
    param_list$contact_points["Lower Sacramento River"] <- round(param_list$contact_points["Lower Sacramento River"] * 1.25) # calibration issue 0.75)
    param_list$contact_points["Lower-mid Sacramento River"] <- round(param_list$contact_points["Lower-mid Sacramento River"] * 1.25) # calibration issue 0.75)
  }
  
  if("SR-4b" %in% action_ids) {
    # reduce prop high predation - effect of modifying regulations on striped bass fishery
    param_list$prop_high_predation["Upper Sacramento River"] <- param_list$prop_high_predation["Upper Sacramento River"] * 1.15 # calibration issue 0.7
    param_list$prop_high_predation["Upper-mid Sacramento River"] <- param_list$prop_high_predation["Upper-mid Sacramento River"] * 1.15 # calibration issue 0.7
    param_list$prop_high_predation["Lower-mid Sacramento River"] <- param_list$prop_high_predation["Lower-mid Sacramento River"] * 1.15 # calibration issue 0.7
    param_list$prop_high_predation["Lower Sacramento River"] <- param_list$prop_high_predation["Lower Sacramento River"] * 1.15 # calibration issue 0.7
  }
  
  if("SR-5" %in% action_ids) {
    # reduce effect by 75% - we saw some effect in the spawners but not too drastic
    param_list$.surv_juv_rear_prop_diversions <- param_list$.surv_juv_rear_prop_diversions * 0.75
  }
  
  if("SR-6" %in% action_ids){
    param_list$gs_bubble_curtain_effect_mult <- 0.8
  }
  
  # SR-7 (DCC gates) got deleted by proponent
  
  if("SR-8" %in% action_ids) {
    # reduce prop high predation
    if("SR-4a" %in% action_ids && "SR-4b" %in% action_ids) {
    param_list$prop_high_predation["Upper Sacramento River"] <- param_list$prop_high_predation["Upper Sacramento River"] * 1.1 # calibration issue 0.7
    param_list$prop_high_predation["Upper-mid Sacramento River"] <- param_list$prop_high_predation["Upper-mid Sacramento River"] * 1.1 # calibration issue 0.7 
    param_list$prop_high_predation["Lower Sacramento River"] <- param_list$prop_high_predation["Lower Sacramento River"] * 1.1 # calibration issue 0.7
    param_list$prop_high_predation["Lower-mid Sacramento River"] <- param_list$prop_high_predation["Lower-mid Sacramento River"] * 1.1 # calibration issue 0.7
    }
    else {
      param_list$prop_high_predation["Upper Sacramento River"] <- param_list$prop_high_predation["Upper Sacramento River"] * 1.3 # calibration issue 0.7
      param_list$prop_high_predation["Upper-mid Sacramento River"] <- param_list$prop_high_predation["Upper-mid Sacramento River"] * 1.3 # calibration issue 0.7 
      param_list$prop_high_predation["Lower Sacramento River"] <- param_list$prop_high_predation["Lower Sacramento River"] * 1.3 # calibration issue 0.7
      param_list$prop_high_predation["Lower-mid Sacramento River"] <- param_list$prop_high_predation["Lower-mid Sacramento River"] * 1.3 # calibration issue 0.7
    }
  }
 
  if("SR-9" %in% action_ids) {
    # spawning habitat: this is an add'l 1.8 acres for 0.75 quantile of spawning habitat
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + habitat_additions$sr_9$spawn
    param_list$abv_dam_spawn_proportion <- mean(habitat_additions$sr_9$spawn / 
                                                  (winterRunDSM::wr_sdm_baseline_params$spawning_habitat["Upper Sacramento River",,] + 
                                                     habitat_additions$sr_9$spawn))
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- param_list$abv_dam_spawn_proportion
    # juvenile survival includes swimming through Lake Shasta
    # adult survival represents survival with volitional challenges
    param_list$dam_passage_survival <- list("adult" = 0.99, "juv" = 0.97)
    if("ASD-6" %in% action_ids) {
      # this parameter now represents volitional passage survival for fish
      # both through Shasta reservoir and thorough keswick, so we scale up 
      # Shasta values (0.8 adult, 0.4 juv) to something in between
      param_list$dam_passage_survival <- list("adult" = 0.8, "juv" = 0.6)
    }
  }
  
  if("SR-10" %in% action_ids) {
    # calculated at 0.75 quantile of habitat to get ~ 1 acre floodplain, ~2 acres inchannel habitat and fry, ~7 acres spawning habitat
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] + (wr_sdm_baseline_params$inchannel_habitat_juvenile["Upper Sacramento River",,] * 1.03)
    param_list$floodplain_habitat["Upper Sacramento River",,] <- param_list$floodplain_habitat["Upper Sacramento River",,] + (wr_sdm_baseline_params$floodplain_habitat["Upper Sacramento River",,] * 1.16)
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + (wr_sdm_baseline_params$spawning_habitat["Upper Sacramento River",,]*1.12)
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + (wr_sdm_baseline_params$inchannel_habitat_fry["Upper Sacramento River",,]*1.03)
    param_list$surv_adult_prespawn_mult["Upper Sacramento River"] <- 1.04
    
  }
  
  if("SR-11" %in% action_ids) {
    param_list$addl_juv_chipps <- 50000
  }
  
  # removed SR-12 because flow action
  
  # Battle Creek--------
  
  # BC-1
  if("BC-1" %in% action_ids) {
    # change the incidental/illegal harvest rate
    param_list$harvest_rate_trib["Battle Creek"] <- param_list$harvest_rate_trib["Battle Creek"] * 0.5
  }
  
  # BC-2
  if("BC-2" %in% action_ids){
    param_list$floodplain_habitat["Battle Creek",,] <- param_list$floodplain_habitat["Battle Creek",,] + habitat_additions$bc_2$fp
    param_list$inchannel_habitat_juvenile["Battle Creek",,] <-param_list$inchannel_habitat_juvenile["Battle Creek",,] + habitat_additions$bc_2$juv
    param_list$inchannel_habitat_fry["Battle Creek",,] <- param_list$inchannel_habitat_fry["Battle Creek",,] + habitat_additions$bc_2$fry
  }
  
  # BC-3 
  # No model inputs
  
  # BC-4 - removed
  
  # BC-5
  if("BC-5" %in% action_ids) {
    param_list$inchannel_habitat_juvenile["Battle Creek",,] <-  param_list$inchannel_habitat_juvenile["Battle Creek",,]+ habitat_additions$bc_5$juv
    param_list$inchannel_habitat_fry["Battle Creek",,] <- param_list$inchannel_habitat_fry["Battle Creek",,] + habitat_additions$bc_5$fry
    param_list$spawning_habitat["Battle Creek",,] <-  param_list$spawning_habitat["Battle Creek",,]+ habitat_additions$bc_5$spawn
  }
  
  if("BC-5" %in% action_ids && "BC-2" %in% action_ids) {
    # add both habitat actions
    param_list$inchannel_habitat_juvenile["Battle Creek",,] <- param_list$inchannel_habitat_juvenile["Battle Creek",,] + habitat_additions$bc_2_5$juv
    param_list$inchannel_habitat_fry["Battle Creek",,] <- param_list$inchannel_habitat_fry["Battle Creek",,] + habitat_additions$bc_2_5$fry
    # BC-5 does not have a fp action, so use bc-2
    param_list$floodplain_habitat["Battle Creek",,] <-  param_list$floodplain_habitat["Battle Creek",,] + habitat_additions$bc_2$fp
    param_list$spawning_habitat["Battle Creek",,] <- param_list$spawning_habitat["Battle Creek",,]  + habitat_additions$bc_2_5$spawn
  }
  
  # BC-6
  # Increase by 3% comes from number of spawning season that is within 5 deg of 53.5
  if("BC-6" %in% action_ids) {
  param_list$spawning_habitat["Battle Creek",,] <- param_list$spawning_habitat["Battle Creek",,]*1.03
  param_list$egg_to_fry_survival_mult["Battle Creek"] <- param_list$egg_to_fry_survival_mult["Battle Creek"] * 1.03
  
  }
  
  # BC-7 
  # Increase by 3% comes from number of spawning season that is within 5 deg of 53.5
  if("BC-7" %in% action_ids) {
    param_list$spawning_habitat["Battle Creek",,] <- param_list$spawning_habitat["Battle Creek",,] *1.03
    param_list$egg_to_fry_survival_mult["Battle Creek"] <- param_list$egg_to_fry_survival_mult["Battle Creek"] * 1.03
  }
  
  # BC-8
  if("BC-8" %in% action_ids) {
    if("BC-9" %in% action_ids) {
      param_list$hatchery_release["Battle Creek","l",] <- param_list$hatchery_release["Battle Creek","l",] *1.1}
    else{
    param_list$hatchery_release["Battle Creek","l",] <- rep(200000, 20)
    }
  }
 
  # TODO modify model code based on a threshold of adults returning to Battle Creek
    # BC-9
  if("BC-9" %in% action_ids) {
    param_list$hatchery_release["Battle Creek","l",] <- rep(200000, 20)
    
    param_list$natural_adult_removal_rate["Battle Creek"] <- 0.25
  }
  
  # Other ---------------------
  # TODO Why are salvaged fish trucked to the South Delta and not Chipps? 
  # O-1 
  if("O-1" %in% action_ids) {
    param_list$delta_survival_multiplier <- 1.01
  }
  
  # O-2
  if("O-2" %in% action_ids) {
    param_list$harvest_rate_ocean <- param_list$harvest_rate_ocean * 0.99
  }
  
  # O-3
  # percent of age class harvested 
  if("O-3" %in% action_ids){
    param_list$harvest_rate_ocean["2"] = param_list$harvest_rate_ocean["2"] * 0.75 # Brad's rec was going from 20% to 15% (25% decrease)
    param_list$harvest_rate_ocean["3"] = param_list$harvest_rate_ocean["3"] * 0.3 # Brad's rec was going from 50% to 15% (70% decrease)
  }
  
  # ASD ------------------
  
  # ASD-1
  if("ASD-1" %in% action_ids) {
    param_list$hatchery_release["Upper Sacramento River","m",] <- rep(20000, 20) + param_list$hatchery_release["Upper Sacramento River","m",]
    param_list$hatchery_release["Upper Sacramento River","l",] <- rep(30000, 20)+ param_list$hatchery_release["Upper Sacramento River","l",]
  }
  
  # ASD-2
  if("ASD-2" %in% action_ids) {
    param_list$harvest_rate_abv_dam <- param_list$harvest_rate_abv_dam * 0.5
  }
  
  # ASD-3 - upper mccloud
  if("ASD-3" %in% action_ids) {
    # multiply only the new hatchery releases for this action by the juvenile capture efficiency
    param_list$juvenile_capture_efficiency_dam_transport <- 0.25
    param_list$hatchery_release["Upper Sacramento River","m",] <- param_list$hatchery_release["Upper Sacramento River","m",] + (c(rep(80000, 8), rep(115000,6), rep(150000, 6))*param_list$juvenile_capture_efficiency_dam_transport)
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$upper_mccloud$fry
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] + habitat_additions$upper_mccloud$juv
    param_list$floodplain_habitat["Upper Sacramento River",,] <- param_list$floodplain_habitat["Upper Sacramento River",,] + habitat_additions$upper_mccloud$fp
    param_list$dam_passage_survival <- list("adult" = 1, "juv" = 0.8)
    param_list$abv_dam_spawn_proportion <- 0
  }
  
  # ASD-4 - lower mccloud
  if("ASD-4" %in% action_ids) {
    param_list$juvenile_capture_efficiency_dam_transport <- 0.25
    param_list$hatchery_release["Upper Sacramento River","m",] <- param_list$hatchery_release["Upper Sacramento River","m",] + (c(rep(80000, 8), rep(115000,6), rep(150000, 6))*param_list$juvenile_capture_efficiency_dam_transport)
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fry
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] + habitat_additions$lower_mccloud$juv
    param_list$floodplain_habitat["Upper Sacramento River",,] <- param_list$floodplain_habitat["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fp
    param_list$dam_passage_survival <- list("adult" = 1, "juv" = 0.8)
    param_list$abv_dam_spawn_proportion <- 0
  }
  
  # cases where we combine ASD-3, ASD-4, and the ASD-5 variants
  # calculate juv habitat separately (was getting too confusing lol)
  # in portfolios, ASD-5a and ASD-5c never get run without ASD-3 and ASD-4
  if("ASD-3" %in% action_ids && "ASD-4" %in% action_ids) {
    # ASD-3 and ASD-4 are always together in portfolios, meaning
    # juvenile habitat will be upper + lower mccloud
    if("ASD-5a" %in% action_ids) {
      # hatchery fry into upper and lower mccloud, + spawners and their fry into lower mccloud
      # so not an exact comparison across ASD-3, ASD-4, and ASD-5
      # keep same for now
    } 
    # no 5-b in portfolios
    if("ASD-5c" %in% action_ids) {
      # just add little sac fry
      param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$little_sac$fry
      param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] + habitat_additions$little_sac$juv
      param_list$floodplain_habitat["Upper Sacramento River",,] <- param_list$floodplain_habitat["Upper Sacramento River",,] + habitat_additions$little_sac$fp
    }
  } else {
    # this is the case where someone is running the function for exploring and don't have
    # ASD-3 and ASD-4 entered. still need to create juv habitat
    if("ASD-5a" %in% action_ids) {
      param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$upper_mccloud$fry
      param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] + habitat_additions$upper_mccloud$juv
      param_list$floodplain_habitat["Upper Sacramento River",,] <- param_list$floodplain_habitat["Upper Sacramento River",,] + habitat_additions$upper_mccloud$fp
    }
    if("ASD-5c" %in% action_ids) {
      param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fry + habitat_additions$little_sac$fry
      param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] + habitat_additions$lower_mccloud$juv+ habitat_additions$little_sac$juv
      param_list$floodplain_habitat["Upper Sacramento River",,] <- param_list$floodplain_habitat["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fp + habitat_additions$little_sac$fp
    }
  }

  # ASD-5 Trap and haul
  # Lower McCloud
  if("ASD-5a" %in% action_ids) {
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + habitat_additions$lower_mccloud$spawn
    # Feedback from participants indicates 0.25 is reasonable value. Model structure can compound reduction of fry, so set artificially high
    param_list$juvenile_capture_efficiency_dam_transport <- 0.9
    # juvenile survival includes transport survival
    param_list$dam_passage_survival <- list("adult" = 0.8, "juv" = 0.8)
    param_list$abv_dam_spawn_proportion <- 0.5
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean(habitat_additions$lower_mccloud$spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+habitat_additions$lower_mccloud$spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.6
    param_list$prespawn_survival_abv_dam <- 0.95
  }
  
  # Little Sac
  if("ASD-5b" %in% action_ids) {
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + habitat_additions$little_sac$spawn
    # Feedback from participants indicates 0.25 is reasonable value. Model structure can compound reduction of fry, so set artificially high
    param_list$juvenile_capture_efficiency_dam_transport <- 0.9
    # juvenile survival includes transport survival
    param_list$dam_passage_survival <- list("adult" = 0.8, "juv" = 0.8)
    param_list$abv_dam_spawn_proportion <- 0.5
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean(habitat_additions$little_sac$spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+habitat_additions$little_sac$spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.6
    param_list$prespawn_survival_abv_dam <- 0.95
  }
  
  # Both
  if("ASD-5c" %in% action_ids) {
    # TODO do we need to set the proportion going to Little Sac and proportion going to McCloud? 
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + habitat_additions$lower_mccloud$spawn + habitat_additions$little_sac$spawn 
    # applied to spawn success
    # Feedback from participants indicates 0.25 is reasonable value. Model structure can compound reduction of fry, so set artificially high
    param_list$juvenile_capture_efficiency_dam_transport <- 0.9
    # juvenile survival includes transport survival
    param_list$dam_passage_survival <- list("adult" = 0.8, "juv" = 0.8)
    param_list$abv_dam_spawn_proportion <- 0.5
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean((habitat_additions$lower_mccloud$spawn+habitat_additions$little_sac$spawn)/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+habitat_additions$lower_mccloud$spawn+habitat_additions$little_sac$spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.6
    param_list$prespawn_survival_abv_dam <- 0.95
  }

  # ASD-6 Volitional both directions
  if("ASD-6" %in% action_ids) {
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + habitat_additions$lower_mccloud$spawn
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fry
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] + habitat_additions$lower_mccloud$juv
    param_list$floodplain_habitat["Upper Sacramento River",,] <- param_list$floodplain_habitat["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fp
    # applied to spawn success
    # juvenile survival includes swimming through Lake Shasta
    # adult survival represents survival with volitional challenges
    param_list$dam_passage_survival <- list("adult" = 0.8, "juv" = 0.4)
    # TODO confirm - we set the abv_dam_spawn_proportion based on habitat for volitional
    param_list$abv_dam_spawn_proportion <- mean(habitat_additions$lower_mccloud$spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+habitat_additions$lower_mccloud$spawn))
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean(habitat_additions$lower_mccloud$spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+habitat_additions$lower_mccloud$spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.6
    param_list$prespawn_survival_abv_dam <- 0.95
  }
  
  # ASD-7 Remove McCloud
  if("ASD-7" %in% action_ids) {
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + habitat_additions$upper_mccloud$spawn
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$upper_mccloud$fry
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] + habitat_additions$upper_mccloud$juv
    param_list$floodplain_habitat["Upper Sacramento River",,] <- param_list$floodplain_habitat["Upper Sacramento River",,] + habitat_additions$upper_mccloud$fp
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean(habitat_additions$full_mccloud$spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+habitat_additions$full_mccloud$spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.7
    param_list$prespawn_survival_abv_dam <- 0.95
  }
  
  # Facilities -------

  # F-1
  # we will use the information provided from temperature modeling to identify the
  # proportion of days that meet the 53.5 F threshold under cold water pool
  # management that did not reach the threshold under baseline management
  if("F-1" %in% action_ids) {
    # 17 months reached 53.5 target in temp control scenario
    # 26 months reached 53.5 target in baseline
    # so moving from 26/252 to 17/252 is an improvement of 0.0357
    # this is compounding on SR-3 which improves by 10%
    param_list$egg_to_fry_survival_mult["Upper Sacramento River"] <- param_list$egg_to_fry_survival_mult["Upper Sacramento River"] * (1 + ((26-17)/252)) 
  }

  return(param_list)
}

#' @title Calculate habitat additions for ASD actions
#' @description Calculates the values of added habitat across ASD actions
#' @export
calculate_habitat_additions_ASD_BC <- function() {
  # baseline objects
  baseline_fry <- DSMhabitat::wr_fry$action_5["Upper Sacramento River",,]
  baseline_juv <- DSMhabitat::wr_juv$action_5["Upper Sacramento River",,]
  baseline_fp <- DSMhabitat::wr_fp$action_5["Upper Sacramento River",,]
  baseline_spawn <- DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]
  
  # upper sac objects (in every TMH object, so we need to subtract), includes baseline
  upper_and_little_sac_fry <- DSMhabitat::wr_fry$action_5_upper_sac_tmh["Upper Sacramento River",,]
  upper_and_little_sac_juv <- DSMhabitat::wr_juv$action_5_upper_sac_tmh["Upper Sacramento River",,]
  upper_and_little_sac_fp <- DSMhabitat::wr_fp$action_5_upper_sac_tmh["Upper Sacramento River",,]
  upper_and_little_sac_spawn <- DSMhabitat::wr_spawn$action_5_upper_sac_tmh["Upper Sacramento River",,]
  
  # mccloud river
  # fry
  upper_sac_w_mccloud_fry <- DSMhabitat::wr_fry$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,]
  mccloud_addition_fry <- upper_sac_w_mccloud_fry - upper_and_little_sac_fry
  upper_mccloud_addition_fry <- mccloud_addition_fry * 0.3 * wr_sdm_temp_habitat_scaling_factors$`Upper McCloud River`$rear
  lower_mccloud_addition_fry <- mccloud_addition_fry * 0.7 * wr_sdm_temp_habitat_scaling_factors$`Lower McCloud River`$rear
  # juv
  upper_sac_w_mccloud_juv <- DSMhabitat::wr_juv$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,]
  mccloud_addition_juv <- upper_sac_w_mccloud_juv - upper_and_little_sac_juv
  upper_mccloud_addition_juv <- mccloud_addition_juv * 0.3 * wr_sdm_temp_habitat_scaling_factors$`Upper McCloud River`$rear
  lower_mccloud_addition_juv <- mccloud_addition_juv * 0.7 * wr_sdm_temp_habitat_scaling_factors$`Lower McCloud River`$rear
  # floodplain
  upper_sac_w_mccloud_fp <- DSMhabitat::wr_fp$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,]
  mccloud_addition_fp <- upper_sac_w_mccloud_fp - upper_and_little_sac_fp
  upper_mccloud_addition_fp <- mccloud_addition_fp * 0.3 * wr_sdm_temp_habitat_scaling_factors$`Upper McCloud River`$rear
  lower_mccloud_addition_fp <- mccloud_addition_fp * 0.7 * wr_sdm_temp_habitat_scaling_factors$`Lower McCloud River`$rear
  # spawn
  upper_sac_w_mccloud_spawn <- DSMhabitat::wr_spawn$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,]
  mccloud_addition_spawn <- upper_sac_w_mccloud_spawn - upper_and_little_sac_spawn
  upper_mccloud_addition_spawn <- mccloud_addition_spawn * 0.3 * wr_sdm_temp_habitat_scaling_factors$`Upper McCloud River`$spawn
  lower_mccloud_addition_spawn <- mccloud_addition_spawn * 0.7 * wr_sdm_temp_habitat_scaling_factors$`Lower McCloud River`$spawn
  
  # temp scale full mccloud addition after it's been used to calculate upper and lower
  mccloud_addition_fry <- mccloud_addition_fry * wr_sdm_temp_habitat_scaling_factors$`Full McCloud River`$rear
  mccloud_addition_juv <- mccloud_addition_juv * wr_sdm_temp_habitat_scaling_factors$`Full McCloud River`$rear
  mccloud_addition_fp <- mccloud_addition_fp * wr_sdm_temp_habitat_scaling_factors$`Full McCloud River`$rear
  mccloud_addition_spawn <- mccloud_addition_spawn * wr_sdm_temp_habitat_scaling_factors$`Full McCloud River`$spawn
  
  # Upper Sacramento - has its own object upper_sac_tmh
  # fry
  little_sac_addition_fry <- (upper_and_little_sac_fry - baseline_fry) * wr_sdm_temp_habitat_scaling_factors$`Little Sacramento River`$rear
  # juv
  little_sac_addition_juv <- (upper_and_little_sac_juv - baseline_juv) * wr_sdm_temp_habitat_scaling_factors$`Little Sacramento River`$rear
  # floodplain
  little_sac_addition_fp <- (upper_and_little_sac_fp - baseline_fp) * wr_sdm_temp_habitat_scaling_factors$`Little Sacramento River`$rear
  # spawn
  little_sac_addition_spawn <- (upper_and_little_sac_spawn - baseline_spawn) * wr_sdm_temp_habitat_scaling_factors$`Little Sacramento River`$spawn
  
  # battle creek
  bc_baseline_spawn <- wr_sdm_habitat_action_5_bc_scaled$spawn["Battle Creek",,]
  bc_baseline_fry <- wr_sdm_habitat_action_5_bc_scaled$fry["Battle Creek",,] 
  bc_baseline_juv <- wr_sdm_habitat_action_5_bc_scaled$juv["Battle Creek",,] 
  bc_baseline_fp <- wr_sdm_habitat_action_5_bc_scaled$fp["Battle Creek",,] 
  
  # bc-2
  bc_bc2_spawn_addition <- (DSMhabitat::wr_spawn$action_5_bc_2["Battle Creek",,] - bc_baseline_spawn) * 
    wr_sdm_temp_habitat_scaling_factors$`Lower Battle Creek`$spawn
  bc_bc2_fry_addition <- (DSMhabitat::wr_fry$action_5_bc_2["Battle Creek",,] - bc_baseline_fry) * 
    wr_sdm_temp_habitat_scaling_factors$`Lower Battle Creek`$rear
  bc_bc2_juv_addition <- (DSMhabitat::wr_juv$action_5_bc_2["Battle Creek",,] - bc_baseline_juv) * 
    wr_sdm_temp_habitat_scaling_factors$`Lower Battle Creek`$rear
  bc_bc2_fp_addition <- (DSMhabitat::wr_fp$action_5_bc_2["Battle Creek",,] - bc_baseline_fp) * 
    wr_sdm_temp_habitat_scaling_factors$`Lower Battle Creek`$rear
  
  # bc-5
  bc_bc5_spawn_addition <- (DSMhabitat::wr_spawn$action_5_bc_5["Battle Creek",,] - bc_baseline_spawn) * 
    wr_sdm_temp_habitat_scaling_factors$`North Fork Battle Creek`$spawn
  bc_bc5_fry_addition <- (DSMhabitat::wr_fry$action_5_bc_5["Battle Creek",,] - bc_baseline_fry) * 
    wr_sdm_temp_habitat_scaling_factors$`North Fork Battle Creek`$rear
  bc_bc5_juv_addition <- (DSMhabitat::wr_juv$action_5_bc_5["Battle Creek",,] - bc_baseline_juv) * 
    wr_sdm_temp_habitat_scaling_factors$`North Fork Battle Creek`$rear
  # no floodplain
  
  # bc-2 and bc-5
  # was using bc_2_bc_5 DSMhabitat object here but it was too much of an issue with the temperature adjustment factors;
  # was reducing the overall value to below the basic BC-5. 
  bc_bc2_bc5_spawn_addition <- bc_bc2_spawn_addition + bc_bc5_spawn_addition
  bc_bc2_bc5_fry_addition <- bc_bc2_fry_addition + bc_bc5_fry_addition
  bc_bc2_bc5_juv_addition <- bc_bc2_juv_addition + bc_bc5_juv_addition
  bc_bc2_bc5_fp_addition <- bc_bc2_fp_addition
  
  
  # sr-9
  # spawning habitat: this is an add'l 1.8 acres for 0.75 quantile of spawning habitat
  sr9_spawn_addition <- baseline_spawn * 0.03
  
  return(list("upper_mccloud" = list("fry" = upper_mccloud_addition_fry,
                                     "juv" = upper_mccloud_addition_juv,
                                     "fp" = upper_mccloud_addition_fp,
                                     "spawn" = upper_mccloud_addition_spawn),
              "lower_mccloud" = list("fry" = lower_mccloud_addition_fry,
                                     "juv" = lower_mccloud_addition_juv,
                                     "fp" = lower_mccloud_addition_fp,
                                     "spawn" = lower_mccloud_addition_spawn),
              "full_mccloud" = list("fry" = mccloud_addition_fry,
                                     "juv" = mccloud_addition_juv,
                                     "fp" = mccloud_addition_fp,
                                     "spawn" = mccloud_addition_spawn),
              "little_sac" = list("fry" = little_sac_addition_fry,
                                  "juv" = little_sac_addition_juv,
                                  "fp" = little_sac_addition_fp,
                                  "spawn" = little_sac_addition_spawn),
              "bc_2" = list("fry" = bc_bc2_fry_addition,
                            "juv" = bc_bc2_juv_addition,
                            "fp" = bc_bc2_fp_addition,
                            "spawn" = bc_bc2_spawn_addition),
              "bc_5" = list("fry" = bc_bc5_fry_addition,
                            "juv" = bc_bc5_juv_addition,
                            "spawn" = bc_bc5_spawn_addition),
              "bc_2_5" = list("fry" = bc_bc2_bc5_fry_addition,
                              "juv" = bc_bc2_bc5_juv_addition,
                              "fp" = bc_bc2_bc5_fp_addition,
                              "spawn" = bc_bc2_bc5_spawn_addition),
              "sr_9" = list("spawn" = sr9_spawn_addition)))
}

#' @title Convert DSMhabitat matrix to tidy df
#' @description Converts a DSM habitat matrix to a tidy df to make plotting and comparison easier
#' @details input matrix should already be indexed for a watershed, giving a matrix of dim 12 x 20 
#' indicating month x sim year. produces results in acres with a column for month and for scenario.
#' @export
convert_hab_matrix_to_df <- function(matrix, scenario) {
  
  include_1979 <- ifelse("1979" %in% colnames(matrix), TRUE, FALSE)
  
  if(include_1979) {
    df <- matrix |> 
      as.data.frame() |> 
      mutate(month = 1:12) |> 
      pivot_longer(`1979`:`2000`,
                   names_to = "year",
                   values_to = "hab_sqm") |> 
      mutate(hab_acres = DSMhabitat::square_meters_to_acres(hab_sqm),
             scenario = scenario) |> 
      select(-hab_sqm)
  } else {
    df <- matrix |> 
      as.data.frame() |> 
      mutate(month = 1:12) |> 
      pivot_longer(`1980`:`2000`,
                   names_to = "year",
                   values_to = "hab_sqm") |> 
      mutate(hab_acres = DSMhabitat::square_meters_to_acres(hab_sqm),
             scenario = scenario) |> 
      select(-hab_sqm)
  }
  final_df <- df |> 
    mutate(plot_date = as.Date(paste0(year, "-", month, "-01")))
  
  return(final_df)
}

