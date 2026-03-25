# action_params <- readr::read_csv("wr_sdm/documentation/WRCS_MASTER_Actions_2026-03-10.csv") |> 
#   janitor::clean_names() |> 
#   dplyr::select(-short_description) |> 
#   tidyr::pivot_longer(hatchery_releases:effect_of_downstream_volitional_passage_on_juvenile_salmon,
#                names_to = "parameter_title",
#                values_to = "action_requires") |> 
#   dplyr::filter(!is.na(action_requires),
#          !is.na(action_id)) |> 
#   dplyr::arrange(parameter_title)

#' @title Create parameter list for portfolios
#' @description Creates updated parameter list for portfolios based on actions included
#' @param action_ids The action ids included in portfolio
#' @export

# impossible_combinations <- c()

create_param_list <- function(action_ids) {
  param_list <- winterRunDSM::wr_sdm_baseline_params
  
  # TODO check that "action_ids" argument does not have any of the impossible combinations
  
  # Data prep
  # TODO rename with _rear or _inchannel
  upper_sac_w_mccloud <- DSMhabitat::wr_fry$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,]
  upper_sac_wo_mccloud <- DSMhabitat::wr_fry$action_5["Upper Sacramento River",,]
  mccloud_addition <- upper_sac_w_mccloud - upper_sac_wo_mccloud
  upper_mccloud_addition <- mccloud_addition * 0.3
  lower_mccloud_addition <- mccloud_addition * 0.7
  
  upper_sac_w_mccloud_spawn <- DSMhabitat::wr_spawn$action_5_upper_sac_mccloud_tmh["Upper Sacramento River",,]
  upper_sac_wo_mccloud_spawn <- DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]
  mccloud_addition_spawn <- upper_sac_w_mccloud_spawn - upper_sac_wo_mccloud_spawn
  upper_mccloud_addition_spawn <- mccloud_addition_spawn * 0.3
  lower_mccloud_addition_spawn <- mccloud_addition_spawn * 0.7
  
  # ds3_inchannel_hab <- DSMhabitat::wr_fry$action_5["Upper Sacramento River",,] + upper_mccloud_addition
  # ds4_inchannel_hab <- DSMhabitat::wr_fry$action_5["Upper Sacramento River",,] + lower_mccloud_addition
  
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
  }
  
  if("SR-2b" %in% action_ids) {
    # double instream habitat in Upper-mid Sacramento River
    # TODO do we need to do anything with temperature suitability here? 
    param_list$inchannel_habitat_juvenile["Upper Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper Sacramento River",,]*2
    param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,] <- param_list$inchannel_habitat_juvenile["Upper-mid Sacramento River",,]*2
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
  
  # BC-3 
  # No model inputs
  
  
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
    param_list$egg_to_fry_survival_abv_dam <- 0.6
  }
  
  # ASD-2
  if("ASD-2" %in% action_ids) {
    param_list$harvest_rate_abv_dam <- param_list$harvest_rate_abv_dam * 0.5
  }
  
  # ASD-3
  if("ASD-3" %in% action_ids) {
    param_list$hatchery_release["Upper Sacramento River","m",] <- c(rep(800000, 8), rep(115000,6), rep(150000, 6))
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + upper_mccloud_addition
    # applied to the hatchery releasesz
    param_list$juvenile_capture_efficiency_dam_transport <- 0.25
    param_list$dam_passage_survival <- list("adult" = 1, "juv" = 0.8)
    param_list$abv_dam_spawn_proportion <- 0
  }
  
  # ASD-4
  if("ASD-4" %in% action_ids) {
    param_list$hatchery_release["Upper Sacramento River","m",] <- c(rep(800000, 8), rep(115000,6), rep(150000, 6))
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + lower_mccloud_addition
    # applied to the hatchery releases
    param_list$juvenile_capture_efficiency_dam_transport <- 0.25
    param_list$dam_passage_survival <- list("adult" = 1, "juv" = 0.8)
    param_list$abv_dam_spawn_proportion <- 0
  }

  # ASD-5 Trap and haul
  if("ASD-5" %in% action_ids) {
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + lower_mccloud_addition_spawn
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + lower_mccloud_addition
    # applied to spawn success
    param_list$juvenile_capture_efficiency_dam_transport <- 0.9
    # juvenile survival includes transport survival
    param_list$dam_passage_survival <- list("adult" = 0.8, "juv" = 0.8)
    param_list$abv_dam_spawn_proportion <- 0.5
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean(lower_mccloud_addition_spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+lower_mccloud_addition_spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.6
  }
  
  
  # TODO Decide between the two
  
  # ASD-6a Volitional on the way up only 
  if("ASD-6a" %in% action_ids) {
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + lower_mccloud_addition_spawn
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + lower_mccloud_addition
    # applied to spawn success
    param_list$juvenile_capture_efficiency_dam_transport <- 0.25
    # juvenile survival includes transport survival
    # adult survival represents transport survival
    param_list$dam_passage_survival <- list("adult" = 0.8, "juv" = 0.8)
    # TODO confirm - we set the abv_dam_spawn_proportion based on habitat for volitional
    param_list$abv_dam_spawn_proportion <-  mean(lower_mccloud_addition_spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+lower_mccloud_addition_spawn))
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean(lower_mccloud_addition_spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+lower_mccloud_addition_spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.6
  }
  
  # ASD-6b Volitional both directions
  if("ASD-6b" %in% action_ids) {
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + lower_mccloud_addition_spawn
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + lower_mccloud_addition
    # applied to spawn success
    # juvenile survival includes swimming through Lake Shasta
    # adult survival represents survival with volitional challenges
    param_list$dam_passage_survival <- list("adult" = 0.8, "juv" = 0.4)
    # TODO confirm - we set the abv_dam_spawn_proportion based on habitat for volitional
    param_list$abv_dam_spawn_proportion <- mean(lower_mccloud_addition_spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+lower_mccloud_addition_spawn))
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean(lower_mccloud_addition_spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+lower_mccloud_addition_spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.6
  }
  
  # ASD-7 Remove McCloud
  if("ASD-7" %in% action_ids) {
    param_list$spawning_habitat["Upper Sacramento River",,] <- param_list$spawning_habitat["Upper Sacramento River",,] + mccloud_addition_spawn
    param_list$inchannel_habitat_fry["Upper Sacramento River",,] <- param_list$inchannel_habitat_fry["Upper Sacramento River",,] + mccloud_addition
    param_list$abv_dam_spawn_habitat_proportion["Upper Sacramento River"] <- mean(mccloud_addition_spawn/ (DSMhabitat::wr_spawn$action_5["Upper Sacramento River",,]+mccloud_addition_spawn))
    param_list$egg_to_fry_survival_abv_dam <- 0.7
  }
  
  # Facilities -------

  # F-1
  # TODO waiting for HEC-5Q
  
  return(param_list)
}
