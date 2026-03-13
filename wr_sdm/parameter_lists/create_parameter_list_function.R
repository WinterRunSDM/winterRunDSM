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

action_params |> filter(action_id == "SR-1")



create_param_list <- function(action_id) {
  param_list <- wr_sdm_baseline_params
  
  #Hatchery -----
  
  # H1
  if(action_id %in% c("H-1")) {
    # change hatchery_release
    param_list$hatchery_release["Upper Sacramento River","l",] <- rep(280000, 20)
    param_list$hatchery_release["Upper-mid Sacramento River","l",] <- rep(90000, 20)
    param_list$hatchery_release["Lower-mid Sacramento River","l",] <- rep(90000, 20)
    param_list$hatchery_release["Lower Sacramento River","l",] <- rep(90000, 20)
  }
  
  # H-2b
  if(action_id %in% c("H-2b")) {
    # change adult removal rate
    param_list$natural_adult_removal_rate["Upper Sacramento River"] <- 0.15
  }
  
  # H-2c
  if(action_id %in% c("H-2c")) {
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
    # double floodplain habitat in Upper and Upper-mid Sacramento River
    param_list$floodplain_habitat["Upper-mid Sacramento River",,] <- param_list$floodplain_habitat["Upper-mid Sacramento River",,]*2
  }
  
  
  if(action_id == "SR-4a") {
    # reduce predator contact points by 25%
    param_list$contact_points <- round(param_list$contact_points * 0.75)
    # TODO explore proportion of high predation. Probably will only choose one of the two parameters.
  }
  
  
  # Above Shasta ----------------
  
  
  
  # Battle Creek--------
  
  # BC-1
  if(action_id == "BC-1") {
    # change the incidental/illegal harvest rate
    param_list$incidental_trib_harvest <- 0.05
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
  
  
  
  
  # Facilities -------
  
  # Other -----------
  
}