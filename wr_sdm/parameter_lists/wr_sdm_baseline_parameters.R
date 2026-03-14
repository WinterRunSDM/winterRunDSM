# remotes::install_github("winterRunSDM/DSMhabitat")
# remotes::install_github("winterRunSDM/DSMflow")
# remotes::install_github("winterRunSDM/DSMtemperature")
library(DSMhabitat)
library(DSMflow)
library(DSMtemperature)
library(tidyverse)


# 2026
# TODO
# Replace with new calibration results
calib_results <- read_rds("calibration/calibration-results-2021-10-11_125845.rds")
solution <- calib_results@solution[1, ]
names(solution) <- NULL

wr_sdm_baseline_params <- list(
  spawn_decay_multiplier = DSMhabitat::spawning_decay_multiplier$biop_itp_2018_2019$wr,
  # Data from DSMscenarios
  # spawn_decay_rate = DSMscenario::spawn_decay_rate,
  # rear_decay_rate = DSMscenario::rear_decay_rate,

  

  # Data from winterRunDSM cache-data (values vary by run)
  prey_density = winterRunDSM::prey_density,
  prey_density_delta = winterRunDSM::prey_density_delta, 
  hatchery_allocation = winterRunDSM::hatchery_allocation,
  natural_adult_removal_rate = winterRunDSM::natural_adult_removal_rate,
  proportion_hatchery = winterRunDSM::proportion_hatchery,
  month_return_proportions = winterRunDSM::month_return_proportions,
  growth_rates = winterRunDSM::growth_rates_inchannel,
  growth_rates_floodplain = winterRunDSM::growth_rates_floodplain,
  mass_by_size_class = winterRunDSM::mass_by_size_class,
  # these do not appear to be used 
  # cross_channel_stray_rate = winterRunDSM::cross_channel_stray_rate,
  # stray_rate = winterRunDSM::stray_rate,
  diversity_group = winterRunDSM::diversity_group,
  
  # Coefficients for adult submodules
  .adult_stray_intercept = 3,
  .adult_stray_wild = -5.5,
  .adult_stray_natal_flow = -1.99,
  .adult_stray_cross_channel_gates_closed = -0.174,
  .adult_stray_prop_bay_trans = 2.09,
  .adult_stray_prop_delta_trans = 2.89,
  .adult_en_route_migratory_temp = -0.26,
  .adult_en_route_bypass_overtopped = -0.019,
  .adult_prespawn_int = 3,
  .adult_prespawn_deg_day = -0.000669526,
  
  # Ocean entry success coefficient and variable
  .ocean_entry_success_length = c(-0.0897309864, -0.0709704348, -0.0208590732, 0.0732620916),
  .ocean_entry_success_months = 0.35,
  
  # Routing coefficients and variables
  .pulse_movement_intercept = -7.70744,
  .pulse_movement_proportion_pulse = 0.26579,
  .pulse_movement_medium = 1.66845,
  .pulse_movement_large = 0.5706,
  .pulse_movement_vlarge = -4.305,
  .pulse_movement_medium_pulse = -0.25477,
  .pulse_movement_large_pulse = -0.44778,
  .pulse_movement_very_large_pulse = 0.329,
  territory_size = c(0.0498944803729701, 0.138941944739835, 0.471083652829798, 0),
  
  # Spawn success variables
  spawn_success_sex_ratio = 0.5,
  spawn_success_redd_size = 9.29,
  spawn_success_fecundity = 5522,
  
  # Egg to fry survival coefficients
  .surv_egg_to_fry_int = 0.041,
  .surv_egg_to_fry_proportion_natural = 0.533,
  .surv_egg_to_fry_scour = -0.655,
  
  # Juvenile rearing survival coefficients and variables
  .surv_juv_rear_contact_points = -0.189, # from literature
  .surv_juv_rear_prop_diversions = -3.51, # from literature
  .surv_juv_rear_total_diversions = -0.0021, # from literature
  .surv_juv_rear_avg_temp_thresh = -0.717,
  .surv_juv_rear_high_predation = -0.122,
  .surv_juv_rear_stranded = -1.939,
  .surv_juv_rear_medium = 1.48,
  .surv_juv_rear_large = 2.223,
  .surv_juv_rear_floodplain = 0.47,
  min_survival_rate = 0.0001,
  
  # Juvenile bypass survival coefficients and variables
  .surv_juv_bypass_avg_temp_thresh = -0.717,
  .surv_juv_bypass_high_predation = -0.122,
  .surv_juv_bypass_medium = 1.48,
  .surv_juv_bypass_large = 2.223,
  .surv_juv_bypass_floodplain = 0.47,
  
  # Juvenile delta survival coefficients and variables
  .surv_juv_delta_avg_temp_thresh = -0.717,
  .surv_juv_delta_contact_points = -0.189,
  .surv_juv_delta_total_diverted = -0.0021,
  .surv_juv_delta_high_predation = -0.122,
  .surv_juv_delta_prop_diverted = -3.51,
  .surv_juv_delta_medium = 1.48,
  .surv_juv_delta_large = 2.223,
  
  ## Variable from load baseline data
  # DSMflow variables -----
  freeport_flows = DSMflow::freeport_flow$action_5,
  vernalis_flows = DSMflow::vernalis_flow$action_5,
  stockton_flows = DSMflow::stockton_flow$action_5,
  CVP_exports = DSMflow::cvp_exports$action_5,
  SWP_exports = DSMflow::swp_exports$action_5,
  proportion_diverted = DSMflow::proportion_diverted$action_5,
  total_diverted = DSMflow::total_diverted$biop_itp_2018_2019,
  delta_proportion_diverted = DSMflow::delta_proportion_diverted$action_5,
  delta_total_diverted = DSMflow::delta_total_diverted$action_5,
  prop_pulse_flows = DSMflow::proportion_pulse_flows$action_5,
  prop_flow_natal = DSMflow::proportion_flow_natal$action_5,
  upper_sacramento_flows = DSMflow::upper_sacramento_flows$action_5,
  delta_inflow = DSMflow::delta_inflow$action_5,
  cc_gates_days_closed = DSMflow::delta_cross_channel_closed$action_5["count", ],
  cc_gates_prop_days_closed = DSMflow::delta_cross_channel_closed$action_5["proportion", ],
  proportion_flow_bypass = DSMflow::proportion_flow_bypasses$action_5,
  gates_overtopped = DSMflow::gates_overtopped$action_5,
  
  # TODO
  # For those without a run, confirm how those are made - do we need to update the modeled values?
  # DSMtemperature variables -----
  vernalis_temps = DSMtemperature::vernalis_temperature,
  prisoners_point_temps = DSMtemperature::prisoners_point_temperature,
  degree_days = DSMtemperature::degree_days$action_5,
  degree_days_abv_dam =  DSMtemperature::degree_days_wr_abv_dam, # winterRunDSM: updated degree days wr for above dam to 13C threshold above dam
  avg_temp = DSMtemperature::stream_temperature$action_5,
  avg_temp_delta = DSMtemperature::delta_temperature,
  migratory_temperature_proportion_over_20 = DSMtemperature::migratory_temperature_proportion_over_20,
  
  # TODO
  # update naming to move away from r_to_r, but keep values
  # biop_itp to change to Action 5
  # DSMhabitat variables -----
  spawning_habitat = DSMhabitat::wr_spawn$action_5,
  inchannel_habitat_fry = DSMhabitat::wr_fry$action_5, # vary by run
  inchannel_habitat_juvenile = DSMhabitat::wr_juv$action_5, # vary by run
  floodplain_habitat = DSMhabitat::wr_fp$action_5, # vary by run
  weeks_flooded = DSMhabitat::weeks_flooded$action_5,
  delta_habitat = DSMhabitat::delta_habitat$action_5,
  sutter_habitat = DSMhabitat::sutter_habitat$action_5,
  yolo_habitat = DSMhabitat::yolo_habitat$action_5,
  tisdale_bypass_watershed = DSMhabitat::tisdale_bypass_watershed,
  yolo_bypass_watershed = DSMhabitat::yolo_bypass_watershed,
  south_delta_routed_watersheds = DSMhabitat::south_delta_routed_watersheds,
  prop_high_predation = DSMhabitat::prop_high_predation,
  contact_points = DSMhabitat::contact_points,
  delta_contact_points = DSMhabitat::delta_contact_points,
  delta_prop_high_predation = DSMhabitat::delta_prop_high_predation,
  prob_strand_early = DSMhabitat::prob_strand_early,
  prob_strand_late = DSMhabitat::prob_strand_late,
  prob_nest_scoured = DSMhabitat::prob_nest_scoured,
  above_dam_spawn_proportion = DSMhabitat::above_dam_spawn_proportion$wr, # R2R - above dam proportion 
  above_dam_rearing_proportion = DSMhabitat::above_dam_rearing_proportion$wr, # R2R - above dam proportion
  
  # Calibration Variables (vary by run)
  # TODO Liz replace the ..surv_egg_to_fry_mean_egg_temp_effect with the correct calibration parameter
  ..surv_egg_to_fry_mean_egg_temp_effect = winterRunDSM::wr_sdm_baseline_params$..surv_egg_to_fry_mean_egg_temp_effect,
  ..surv_adult_enroute_int = solution[1],
  ..surv_juv_rear_int = rep(solution[2], 31),
  ..surv_juv_rear_contact_points = solution[3],
  ..surv_juv_rear_prop_diversions = solution[4],
  ..surv_juv_rear_total_diversions = solution[5],
  ..surv_juv_bypass_int = solution[6],
  ..surv_juv_delta_int = solution[7],
  ..surv_juv_delta_contact_points = solution[8],
  ..surv_juv_delta_total_diverted = solution[9],
  ..surv_juv_outmigration_sj_int = solution[10],
  ..ocean_entry_success_int = rep(solution[11], 31),
  
  # TODO 
  # R2R specific metrics
  hatchery_release = winterRunDSM::winter_hatchery_release, 
  hatchery_release_proportion_bay = winterRunDSM::hatchery_release_proportion_bay, 
  fecundity_lookup = winterRunDSM::fecundity_by_age,
  
  # TODO
  # Change biop to Action5
  # Flows for stray 
  # flows_oct_nov = DSMflow::hatchery_oct_nov_flows$action_5,
  # flows_apr_may = DSMflow::hatchery_apr_may_flows$action_5,
  
  # multi route
  movement_hypo_weights = c(1, rep(0, 7)), # rep(1/8, 8),
  ..habitat_capacity = 5,
  ..floodplain_capacity = 5,
  
  # winter Run SDM inputs
  adult_enroute_surv_mult = setNames(c(1, rep(1, 30)), winterRunDSM::watershed_labels),
  harvest_rate_ocean = 0.11,
  harvest_rate_trib = setNames(rep(0.1, 31), winterRunDSM::watershed_labels),
  # survival estimate for adults - volitional passage past Keswick Dam
  effect_upstream_vol_adult_kwk = 0,
  # survival estimate for juveniles - volitional passage past Keswick Dam
  effect_upstream_vol_juv_kwk = 0,
  addl_juv_chipps = 0
)

usethis::use_data(wr_sdm_baseline_params, overwrite = TRUE)
