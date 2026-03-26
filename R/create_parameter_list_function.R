#' @title Create parameter list for portfolios
#' @description Creates a new parameter list for input to the winter run model that uses
  #' `winterRunDSM::wr_sdm_baseline_params` as a basis.
#' @param action_ids A vector of action IDs to represent in the parameters. 
#' @export
create_param_list <- function(action_ids) {
  param_list <- winterRunDSM::wr_sdm_baseline_params
  
  # TODO check that "action_ids" argument does not have any of the impossible combinations
  
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
    param_list$adult_enroute_surv_mult["Upper Sacramento River"] <- param_list$adult_enroute_surv_mult["Upper Sacramento River"] * 1.1
  }
  
  # Sacramento River ----------------
  if("SR-1" %in% action_ids) {
    # triple floodplain habitat in Upper-mid Sacramento River
    # TODO this causes a negative effect in years 13:15 that reduces overall spawners in final years
    param_list$floodplain_habitat["Upper-mid Sacramento River",,] <- param_list$floodplain_habitat["Upper-mid Sacramento River",,]*3
  }
  
  if("SR-2a" %in% action_ids) {
    # double instream habitat in Upper-mid Sacramento River
    # TODO do we need to do anything with temperature suitability here? 
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,]*2
    param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,]*2
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,]*2
    param_list$inchannel_habitat_fry["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper-mid Sacramento River",,]*2
  }
  
  if("SR-2b" %in% action_ids) {
    # double instream habitat in Upper-mid Sacramento River
    # TODO do we need to do anything with temperature suitability here? 
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,]*2
    param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,]*2
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,]*2
    param_list$inchannel_habitat_fry["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper-mid Sacramento River",,]*2
  }
  
  if("SR-2c" %in% action_ids) {
    # TODO add rearing habitat
    param_list$non_natal_proportion_shift <- 0.6 # could be 45% - 60%
  }
  
  if("SR-3" %in% action_ids) {
    # TODO add spawning habitat; adjust for temperature?
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
    param_list$prop_high_predation["Upper Sacramento River"] <- param_list$prop_high_predation["Upper Sacramento River"] * 1.3 # calibration issue 0.7
    param_list$prop_high_predation["Upper-mid Sacramento River"] <- param_list$prop_high_predation["Upper-mid Sacramento River"] * 1.3 # calibration issue 0.7
    param_list$prop_high_predation["Lower-mid Sacramento River"] <- param_list$prop_high_predation["Lower-mid Sacramento River"] * 1.3 # calibration issue 0.7
    param_list$prop_high_predation["Lower Sacramento River"] <- param_list$prop_high_predation["Lower Sacramento River"] * 1.3 # calibration issue 0.7
  }
  
  if("SR-5" %in% action_ids) {
    # reduce effect by 75% - we saw some effect in the spawners but not too drastic
    param_list$.surv_juv_rear_prop_diversions <- param_list$.surv_juv_rear_prop_diversions * 0.75
  }
  
  if("SR-6" %in% action_ids){
    # Change routing for Georgiana Slough
    param_list$gs_bubble_curtain_effect_mult <- 0.8
  }
  
  # SR-7 (DCC gates) got deleted by proponent
  
  if("SR-8a" %in% action_ids) {
    # TODO TBD on the reduction percentage
    param_list$contact_points <- round(param_list$contact_points * 0.5)
  }
  
  if("SR-8b" %in% action_ids) {
    # reduce prop high predation
    param_list$prop_high_predation["Upper Sacramento River"] <- 0.2 
  }
 
  if("SR-9" %in% action_ids) {
    # TODO implement this change in model script? or spawn_success? 
    param_list$effect_upstream_vol_adult_kwk <- 0.99
    param_list$effect_upstream_vol_juv_kwk <- 0.97
    # TODO modify this value
    param_list$spawning_habitat["Upper-mid Sacramento River",,] <- param_list$spawning_habitat["Upper-mid Sacramento River",,] + 4
  }
  
  
  if("SR-11" %in% action_ids) {
    param_list$addl_juv_chipps <- 50000
  }
  
  # TODO wait on Rene
  if("SR-12" %in% action_ids) {
  }
  
  # Above Shasta ----------------
  # Battle Creek--------
  
  # BC-1
  # Right now we are only including ocean harvest
  if("BC-1" %in% action_ids) {
    # change the incidental/illegal harvest rate
    param_list$harvest_rate_trib["Battle Creek"] <- param_list$harvest_rate_trib["Battle Creek"] * 0.5
  }
  
  # BC-2
  if("BC-2" %in% action_ids){
    param_list$floodplain_habitat["Battle Creek",,] <- wr_sdm_habitat_action_5_bc_scaled$fp["Battle Creek",,] + habitat_additions$bc_2$fp
    param_list$inchannel_habitat_juvenile["Battle Creek",,] <- wr_sdm_habitat_action_5_bc_scaled$juv["Battle Creek",,] + habitat_additions$bc_2$juv
    param_list$inchannel_habitat_fry["Battle Creek",,] <- wr_sdm_habitat_action_5_bc_scaled$fry["Battle Creek",,] + habitat_additions$bc_2$fry
  }
  
  # BC-3 
  # No model inputs
  
  # BC-4 - removed
  
  # BC-5
  if("BC-5" %in% action_ids) {
    param_list$inchannel_habitat_juvenile["Battle Creek",,] <- wr_sdm_habitat_action_5_bc_scaled$juv["Battle Creek",,] + habitat_additions$bc_5$juv
    param_list$inchannel_habitat_fry["Battle Creek",,] <- wr_sdm_habitat_action_5_bc_scaled$fry["Battle Creek",,] + habitat_additions$bc_5$fry
    param_list$spawning_habitat["Battle Creek",,] <- wr_sdm_habitat_action_5_bc_scaled$spawn["Battle Creek",,] + habitat_additions$bc_5$spawn
  }
  
  if("BC-5" %in% action_ids && "BC-2" %in% action_ids) {
    # add both habitat actions
    param_list$inchannel_habitat_juvenile["Battle Creek",,] <- wr_sdm_habitat_action_5_bc_scaled$juv["Battle Creek",,] + habitat_additions$bc_2_5$juv
    param_list$inchannel_habitat_fry["Battle Creek",,] <- wr_sdm_habitat_action_5_bc_scaled$fry["Battle Creek",,] + habitat_additions$bc_2_5$fry
    # BC-5 does not have a fp action, so use bc-2
    param_list$floodplain_habitat["Battle Creek",,] <- wr_sdm_habitat_action_5_bc_scaled$fp["Battle Creek",,] + habitat_additions$bc_2$fp
    param_list$spawning_habitat["Battle Creek",,] <- wr_sdm_habitat_action_5_bc_scaled$spawn["Battle Creek",,] + habitat_additions$bc_2_5$spawn
  }
  
  # BC-6 - are we doing this?
  
  # BC-8
  if("BC-8" %in% action_ids) {
    param_list$hatchery_release["Battle Creek","l",] <- rep(200000, 20)
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
  # change the incidental/illegal harvest rate
    param_list$harvest_rate_ocean <- param_list$harvest_rate_ocean * 0.99
    }
  
  # O-3 
  # TODO 
  # percent of age class harvested 
  # if("O-3"){
  #   
  # }
  
  # ASD ------------------
  
  # ASD-1
  if("ASD-1" %in% action_ids) {
    param_list$hatchery_release["Upper Sacramento River","m",] <- rep(300000, 20)
    param_list$hatchery_release["Upper Sacramento River","l",] <- rep(200000, 20)
  }
  
  # ASD-2
  if("ASD-2" %in% action_ids) {
    param_list$harvest_rate_abv_dam <- param_list$harvest_rate_abv_dam * 0.5
  }
  
  # ASD-3
  if("ASD-3" %in% action_ids) {
    param_list$hatchery_release["Upper Sacramento River","m",] <- c(rep(80000, 8), rep(115000,6), rep(150000, 6))
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$upper_mccloud$fry
    param_list$inchannel_habitat_juv["Upper Sacramento River",,] <- param_list$inchannel_habitat_juv["Upper Sacramento River",,] + habitat_additions$upper_mccloud$juv
    param_list$inchannel_habitat_fp["Upper Sacramento River",,] <- param_list$inchannel_habitat_fp["Upper Sacramento River",,] + habitat_additions$upper_mccloud$fp
    # applied to the hatchery releases
    param_list$juvenile_capture_efficiency_dam_transport <- 0.25
    param_list$dam_passage_survival <- list("adult" = 1, "juv" = 0.8)
  }
  
  # ASD-4
  if("ASD-4" %in% action_ids) {
    param_list$hatchery_release["Upper Sacramento River","m",] <- c(rep(800000, 8), rep(115000,6), rep(150000, 6))
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fry
    param_list$inchannel_habitat_juv["Upper Sacramento River",,] <- param_list$inchannel_habitat_juv["Upper Sacramento River",,] + habitat_additions$lower_mccloud$juv
    param_list$inchannel_habitat_fp["Upper Sacramento River",,] <- param_list$inchannel_habitat_fp["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fp
    # applied to the hatchery releases
    param_list$juvenile_capture_efficiency_dam_transport <- 0.25
    param_list$dam_passage_survival <- list("adult" = 1, "juv" = 0.8)
    param_list$abv_dam_spawn_proportion <- 0
  }

  # ASD-5 Trap and haul
  if("ASD-5" %in% action_ids) {
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + habitat_additions$lower_mccloud$spawn
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fry
    param_list$inchannel_habitat_juv["Upper Sacramento River",,] <- param_list$inchannel_habitat_juv["Upper Sacramento River",,] + habitat_additions$lower_mccloud$juv
    param_list$inchannel_habitat_fp["Upper Sacramento River",,] <- param_list$inchannel_habitat_fp["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fp
    # applied to spawn success
    param_list$juvenile_capture_efficiency_dam_transport <- 0.9
    # juvenile survival includes transport survival
    param_list$dam_passage_survival <- list("adult" = 0.8, "juv" = 0.8)
    param_list$abv_dam_spawn_proportion <- 0.5
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean(habitat_additions$lower_mccloud$spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,] + habitat_additions$lower_mccloud$spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.6
    param_list$prespawn_survival_abv_dam <- 0.95
  }
  
  
  # TODO Decide between the two
  
  # ASD-6a Volitional on the way up only 
  if("ASD-6a" %in% action_ids) {
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + habitat_additions$lower_mccloud$spawn
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fry
    param_list$inchannel_habitat_juv["Upper Sacramento River",,] <- param_list$inchannel_habitat_juv["Upper Sacramento River",,] + habitat_additions$lower_mccloud$juv
    param_list$inchannel_habitat_fp["Upper Sacramento River",,] <- param_list$inchannel_habitat_fp["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fp
    # applied to spawn success
    param_list$juvenile_capture_efficiency_dam_transport <- 0.25
    # juvenile survival includes transport survival
    # adult survival represents transport survival
    param_list$dam_passage_survival <- list("adult" = 0.8, "juv" = 0.8)
    # TODO confirm - we set the abv_dam_spawn_proportion based on habitat for volitional
    param_list$abv_dam_spawn_proportion <-  mean(habitat_additions$lower_mccloud$spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+habitat_additions$lower_mccloud$spawn))
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean(habitat_additions$lower_mccloud$spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+habitat_additions$lower_mccloud$spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.6
    param_list$prespawn_survival_abv_dam <- 0.95
  }
  
  # ASD-6b Volitional both directions
  if("ASD-6b" %in% action_ids) {
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + habitat_additions$lower_mccloud$spawn
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fry
    param_list$inchannel_habitat_juv["Upper Sacramento River",,] <- param_list$inchannel_habitat_juv["Upper Sacramento River",,] + habitat_additions$lower_mccloud$juv
    param_list$inchannel_habitat_fp["Upper Sacramento River",,] <- param_list$inchannel_habitat_fp["Upper Sacramento River",,] + habitat_additions$lower_mccloud$fp
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
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + habitat_additions$full_mccloud$spawn
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + habitat_additions$full_mccloud$fry
    param_list$inchannel_habitat_juv["Upper Sacramento River",,] <- param_list$inchannel_habitat_juv["Upper Sacramento River",,] + habitat_additions$full_mccloud$juv
    param_list$inchannel_habitat_fp["Upper Sacramento River",,] <- param_list$inchannel_habitat_fp["Upper Sacramento River",,] + habitat_additions$full_mccloud$fp
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean(habitat_additions$full_mccloud$spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+habitat_additions$full_mccloud$spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.7
    param_list$prespawn_survival_abv_dam <- 0.95
  }
  
  # Facilities -------

  # F-1
  # TODO waiting for HEC-5Q
  
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
  
  # mccloud river
  # fry
  upper_sac_w_mccloud_fry <- DSMhabitat::wr_fry$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,]
  mccloud_addition_fry <- upper_sac_w_mccloud_fry - baseline_fry
  upper_mccloud_addition_fry <- mccloud_addition_fry * 0.3 * wr_sdm_temp_habitat_scaling_factors$`Upper McCloud River`$rear
  lower_mccloud_addition_fry <- mccloud_addition_fry * 0.7 * wr_sdm_temp_habitat_scaling_factors$`Lower McCloud River`$rear
  # juv
  upper_sac_w_mccloud_juv <- DSMhabitat::wr_juv$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,]
  mccloud_addition_juv <- upper_sac_w_mccloud_juv - baseline_juv
  upper_mccloud_addition_juv <- mccloud_addition_juv * 0.3 * wr_sdm_temp_habitat_scaling_factors$`Upper McCloud River`$rear
  lower_mccloud_addition_juv <- mccloud_addition_juv * 0.7 * wr_sdm_temp_habitat_scaling_factors$`Lower McCloud River`$rear
  # floodplain
  upper_sac_w_mccloud_fp <- DSMhabitat::wr_fp$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,]
  mccloud_addition_fp <- upper_sac_w_mccloud_fp - baseline_fp
  upper_mccloud_addition_fp <- mccloud_addition_fp * 0.3 * wr_sdm_temp_habitat_scaling_factors$`Upper McCloud River`$rear
  lower_mccloud_addition_fp <- mccloud_addition_fp * 0.7 * wr_sdm_temp_habitat_scaling_factors$`Lower McCloud River`$rear
  # spawn
  upper_sac_w_mccloud_spawn <- DSMhabitat::wr_spawn$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,]
  mccloud_addition_spawn <- upper_sac_w_mccloud_spawn - baseline_spawn
  upper_mccloud_addition_spawn <- mccloud_addition_spawn * 0.3 * wr_sdm_temp_habitat_scaling_factors$`Upper McCloud River`$spawn
  lower_mccloud_addition_spawn <- mccloud_addition_spawn * 0.7 * wr_sdm_temp_habitat_scaling_factors$`Lower McCloud River`$spawn
  
  # temp scale full mccloud addition after it's been used to calculate upper and lower
  mccloud_addition_fry <- mccloud_addition_fry * wr_sdm_temp_habitat_scaling_factors$`Full McCloud River`$rear
  mccloud_addition_juv <- mccloud_addition_juv * wr_sdm_temp_habitat_scaling_factors$`Full McCloud River`$rear
  mccloud_addition_fp <- mccloud_addition_fp * wr_sdm_temp_habitat_scaling_factors$`Full McCloud River`$rear
  mccloud_addition_spawn <- mccloud_addition_spawn * wr_sdm_temp_habitat_scaling_factors$`Full McCloud River`$spawn
  
  # Upper Sacramento - needs to be calculated from full TMH minus (-) Pit/McCloud
  # fry
  upper_sac_w_little_sac_fry <- DSMhabitat::wr_fry$action_5_upper_sac_tmh["Upper Sacramento River",,] - 
    DSMhabitat::wr_fry$action_5_upper_sac_pit_mccloud_tmh["Upper Sacramento River",,]
  little_sac_addition_fry <- (upper_sac_w_little_sac_fry - baseline_fry) * wr_sdm_temp_habitat_scaling_factors$`Little Sacramento River`$rear
  # juv
  upper_sac_w_little_sac_juv <- DSMhabitat::wr_juv$action_5_upper_sac_tmh["Upper Sacramento River",,] - 
    DSMhabitat::wr_juv$action_5_upper_sac_pit_mccloud_tmh["Upper Sacramento River",,]
  little_sac_addition_juv <- (upper_sac_w_little_sac_juv - baseline_fry) * wr_sdm_temp_habitat_scaling_factors$`Little Sacramento River`$rear
  # floodplain
  upper_sac_w_little_sac_fp <- DSMhabitat::wr_fp$action_5_upper_sac_tmh["Upper Sacramento River",,] - 
    DSMhabitat::wr_fp$action_5_upper_sac_pit_mccloud_tmh["Upper Sacramento River",,]
  little_sac_addition_fp <- (upper_sac_w_little_sac_fp - baseline_fp) * wr_sdm_temp_habitat_scaling_factors$`Little Sacramento River`$rear
  # spawn
  upper_sac_w_little_sac_spawn <- DSMhabitat::wr_spawn$action_5_upper_sac_tmh["Upper Sacramento River",,] - 
    DSMhabitat::wr_spawn$action_5_upper_sac_pit_mccloud_tmh["Upper Sacramento River",,]
  little_sac_addition_spawn <- (upper_sac_w_little_sac_spawn - baseline_spawn) * wr_sdm_temp_habitat_scaling_factors$`Little Sacramento River`$spawn
  
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
  bc_bc2_bc5_spawn_addition <- (DSMhabitat::wr_spawn$action_5_bc_2_bc_5["Battle Creek",,] - bc_baseline_spawn) * 
    mean(wr_sdm_temp_habitat_scaling_factors$`Lower Battle Creek`$spawn, 
         wr_sdm_temp_habitat_scaling_factors$`North Fork Battle Creek`$spawn)
  bc_bc2_bc5_fry_addition <- (DSMhabitat::wr_fry$action_5_bc_2_bc_5["Battle Creek",,] - bc_baseline_fry) * 
    mean(wr_sdm_temp_habitat_scaling_factors$`Lower Battle Creek`$rear, 
         wr_sdm_temp_habitat_scaling_factors$`North Fork Battle Creek`$rear)
  bc_bc2_bc5_juv_addition <- (DSMhabitat::wr_juv$action_5_bc_2_bc_5["Battle Creek",,] - bc_baseline_juv) * 
    mean(wr_sdm_temp_habitat_scaling_factors$`Lower Battle Creek`$rear, 
         wr_sdm_temp_habitat_scaling_factors$`North Fork Battle Creek`$rear)
  bc_bc2_bc5_fp_addition <- (DSMhabitat::wr_fp$action_5_bc_2["Battle Creek",,] - bc_baseline_fp) * 
    wr_sdm_temp_habitat_scaling_factors$`Lower Battle Creek`$rear
  
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
                              "spawn" = bc_bc2_bc5_spawn_addition)))
}
