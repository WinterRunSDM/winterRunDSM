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


create_param_list <- function(action_id) {
  param_list <- wr_sdm_baseline_params
  
  # adult enroute survival
  if(action_id == "H-3") {
    # through explorations, this has a significant effect on adult returns. need to be cautious
    param_list$adult_enroute_surv_mult["Upper Sacramento River"] <- param_list$adult_enroute_surv_mult["Upper Sacramento River"] * 1.1
  }
}